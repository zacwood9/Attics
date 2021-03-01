//
//  MusicPlayer.swift
//  Attics
//
//  Created by Zachary Wood on 11/3/20.
//

import AVKit
import MediaPlayer
import SwiftUI
import Combine

struct Playlist: Codable {
    var band: Band
    var show: Show
    var source: Source
    var songs: [Song]
}

class MusicPlayer : ObservableObject {
    struct State: Codable {
        var song: Song
        var playlist: Playlist
        var time: Double
        var duration: Double
        var playing: Bool
        
        func songIndex() -> Int {
            guard let index = playlist.songs.firstIndex(where: { $0.id == song.id }) else {
                fatalError("Coudln't find song in current playlist?")
            }
            return index
        }
        
        fileprivate mutating func nextTrack() {
            let index = songIndex()
            if index + 1 >= playlist.songs.count { // end of playlist
                song = playlist.songs[0] // go to beginning
            } else {
                song = playlist.songs[index + 1] // next track
            }
        }
        
        fileprivate mutating func previousTrack() {
            let index = songIndex()
            guard index - 1 >= 0 else { return } // if we're at the beginning, do nothing
            song = playlist.songs[index - 1] // previous song
        }
        
        fileprivate mutating func seek(to time: Double) {
            self.time = time
        }
        
        fileprivate mutating func pause() {
            playing.toggle()
        }
        
        mutating func setToIndex(_ index: Int) {
            song = playlist.songs[index]
        }
    }
    
    @Published var state: State? = nil
    @Published var showPopup: Bool = false
    
    
    func start(_ song: Song, _ playlist: Playlist) {
        state = State(song: song, playlist: playlist, time: 0, duration: 0, playing: true)
        initializePlayer()
    }
    
    func resume() {
        guard state != nil else { fatalError(".resume action called without state") }
        guard state!.playing == false else { return }
        state!.playing = true
        player.play()
    }
    
    func nextTrack() {
        guard state != nil else { fatalError(".nextTrackForce action called without state") }
        state!.nextTrack()
        state!.playing = true
        initializePlayer()
    }
    
    func previousTrack() {
        guard state != nil else { fatalError(".previousTrack action called without state") }
        state!.previousTrack()
        state!.playing = true
        initializePlayer()
    }
    
    func seek(_ time: Double) {
        guard state != nil else { fatalError(".seek action called without state") }
        state!.seek(to: time)
    }
    
    func pause() {
        guard state != nil else { fatalError(".pause action called without state") }
        state!.playing = false
        player.pause()
        
    }
    
    func restore(_ s: State) {
        guard state == nil else { return }
        state = s
        initializePlayer()
        player.pause()
        player.seek(to: CMTime(seconds: s.time, preferredTimescale: 100))
    }
    
    private var player: AVQueuePlayer = AVQueuePlayer(playerItem: nil)
    private var playerItems: [AVPlayerItem] = []
    private var storage: AppStorageManager
    
    var viewIsSeeking = false {
        didSet {
            updateSeeker()
        }
    }
    
    @Published var percentFinished: Double = 0
    
    private var itemStarted: AnyCancellable?
    
    private init(storage: AppStorageManager) {
        self.storage = storage
        setupRemoteControls()
        
        itemStarted = player.publisher(for: \.currentItem, options: .new)
            .sink(receiveValue: { item in
                if let item = item,
                   let index = self.playerItems.firstIndex(of: item) {
                    self.state?.setToIndex(index)
                }
                self.setupNowPlaying()
            })
        
        player.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1.0, preferredTimescale: 100), queue: .main) { time in
            self.updateSeeker()
            self.setupNowPlaying()
        }
        
        interruptionObserver = NotificationCenter.default.publisher(for: AVAudioSession.interruptionNotification).sink { [weak self] _ in
            self?.pause()
        }
    }
    
    static var shared = MusicPlayer(storage: AppStorageManager.shared)
    
    private var songEndedObserver: AnyCancellable?
    private var interruptionObserver: AnyCancellable?

    private func initializePlayer() {
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
        
        guard let state = state else { return }
        player.removeAllItems()
        
        playerItems = state.playlist.songs.map { song in
            if let url = storage.getLocalSongUrl(recording: state.playlist.source, song: song) {
                return AVPlayerItem(url: url)
            }
            let identifier = state.playlist.source.identifier
            let fileName = song.fileName
            let downloadUrl = "https://archive.org/download/\(identifier)/\(fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
            let url = URL(string: downloadUrl)!
            return AVPlayerItem(url: url)
        }
        
        let followingItems = playerItems[state.songIndex() ..< playerItems.count]
        followingItems.forEach { player.insert($0, after: nil) }
        player.play()
    }

    func updateSeeker() {
        guard let item = player.currentItem, !viewIsSeeking else { return }
        
        state?.time = player.currentTime().seconds
        state?.duration = item.duration.seconds
        percentFinished = player.currentTime().seconds / item.duration.seconds
    }
    
    func seekStateDidChange(_ isSeeking: Bool) {
        if !isSeeking {
            if let totalSeconds = player.currentItem?.duration.seconds {
                let seconds = totalSeconds * percentFinished
                player.seek(to: CMTime(seconds: seconds, preferredTimescale: 100)) { _ in
                    self.viewIsSeeking = isSeeking
                }
            }
        } else {
            self.viewIsSeeking = isSeeking
        }
    }
    
    var currentTime: String {
        if let item = player.currentItem {
            if viewIsSeeking && item.duration.isNumeric {
                return (item.duration.seconds * percentFinished).timeString
            }
            else {
                if item.currentTime().isNumeric {
                    return item.currentTime().seconds.timeString
                }
                return "..."
            }
        } else {
            return "..."
        }
    }
    
    var duration: String {
        if let item = player.currentItem, item.duration.isNumeric {
            return item.duration.seconds.timeString
        } else {
            return "..."
        }
    }
    
    func encodeState() throws -> Data? {
        if let state = state {
            let encoder = JSONEncoder()
            encoder.keyEncodingStrategy = .convertToSnakeCase
            return try encoder.encode(state)
        }
        return nil
    }
    
    private func setupRemoteControls() {
        let commandCenter = MPRemoteCommandCenter.shared()
        
        commandCenter.playCommand.addTarget { [weak self] _ in
            guard let self = self, let state = self.state else { return .noActionableNowPlayingItem }
            
            if !state.playing {
                self.resume()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.pauseCommand.addTarget { [weak self] _ in
            guard let self = self, let state = self.state else { return .noActionableNowPlayingItem }
            
            if state.playing {
                self.pause()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.nextTrackCommand.addTarget { [weak self] _ in
            guard let self = self else { return .noActionableNowPlayingItem }
            self.nextTrack()
            return .success
        }
        
        commandCenter.previousTrackCommand.addTarget { [weak self] _ in
            guard let self = self else { return .noActionableNowPlayingItem }
            self.previousTrack()
            return .success
        }
        
        commandCenter.changePlaybackPositionCommand.addTarget { [weak self] (event) -> MPRemoteCommandHandlerStatus in
            guard let self = self else { return .noActionableNowPlayingItem }
            
            if let event = event as? MPChangePlaybackPositionCommandEvent {
                self.player.seek(to: CMTime(seconds: event.positionTime, preferredTimescale: 100))
                return .success
            }
            #colorLiteral(red: 0.2, green: 0.2666666667, blue: 0.5490196078, alpha: 1)
            return .commandFailed
        }
    }
    
    func setupNowPlaying() {
        guard let state = state else { return }
        let song = state.song
        
        let info = Info(
            title: song.title,
            currentTime: player.currentTime().seconds,
            duration: player.currentItem?.duration.seconds ?? 0.0,
            rate: player.rate,
            artist: state.playlist.band.name,
            album: song.album
        )
        
        MPNowPlayingInfoCenter.default().nowPlayingInfo = info.toNowPlayingInfo()
    }
}

struct Info {
    let title: String
    let currentTime: Double
    let duration: Double
    let rate: Float
    let artist: String
    let album: String
}

extension Info {
    func toNowPlayingInfo() -> [String : Any] {
        var nowPlayingInfo = [String : Any]()
        nowPlayingInfo[MPMediaItemPropertyTitle] = title
        
        if let image = UIImage(named: "lockscreen") {
            nowPlayingInfo[MPMediaItemPropertyArtwork] =
                MPMediaItemArtwork(boundsSize: image.size) { size in
                    return image
                }
        }
        
        nowPlayingInfo[MPNowPlayingInfoPropertyElapsedPlaybackTime] = currentTime
        nowPlayingInfo[MPMediaItemPropertyPlaybackDuration] = duration
        nowPlayingInfo[MPNowPlayingInfoPropertyPlaybackRate] = rate
        nowPlayingInfo[MPMediaItemPropertyArtist] = artist
        nowPlayingInfo[MPMediaItemPropertyAlbumTitle] = album
        
        return nowPlayingInfo
    }
}
