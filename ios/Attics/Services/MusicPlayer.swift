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
    enum Action {
        case play(Song, Playlist)
        case nextTrack
        case previousTrack
        case seek(Double)
        case pause
        case resume
    }
    
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
            
        }
        
        fileprivate mutating func pause() {
            playing.toggle()
        }
    }
    
    @Published var state: State? = nil
    @Published var showPopup: Bool = false
    
    func dispatch(action: Action) {
        switch action {
        case .play(let song, let playlist):
            state = State(song: song, playlist: playlist, time: 0, duration: 0, playing: true)
            startPlayer()
            
        case .resume:
            guard state != nil else { fatalError(".resume action called without state") }
            guard state!.playing == false else { return }
            state!.playing = true
            player.play()
            
        case .nextTrack:
            guard state != nil else { fatalError(".nextTrack action called without state") }
            state!.nextTrack()
            state!.playing = true
            startPlayer()
            
        case .previousTrack:
            guard state != nil else { fatalError(".previousTrack action called without state") }
            state!.previousTrack()
            state!.playing = true
            startPlayer()
            
        case .seek(let time):
            guard state != nil else { fatalError(".seek action called without state") }
            state!.seek(to: time)
            
        case .pause:
            guard state != nil else { fatalError(".pause action called without state") }
            state!.pause()
            player.pause()
        }
    }
    
    private var player: AVPlayer = AVPlayer(playerItem: nil)
    private var item: AVPlayerItem? = nil
    private var storage: AppStorageManager
    
    var viewIsSeeking = false {
        didSet {
            updateSeeker()
        }
    }
    
    @Published var percentFinished: Double = 0
    
    init(storage: AppStorageManager) {
        self.storage = storage
        setupRemoteControls()
    }
    
    private var songEndedObserver: AnyCancellable?
    private var interruptionObserver: AnyCancellable?
    
    private func startPlayer() {
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
        player.pause()
        
        guard let state = state else { fatalError("Attempted to start player without state") }
        // at this point, we have a new state, nothing is loaded
        
        // try to get the song from disk
        if let url = storage.getLocalSongUrl(recording: state.playlist.source, song: state.song) {
            print("LOG: Playing from disk")
            item = AVPlayerItem(url: url)
            player = AVPlayer(playerItem: item)
            player.automaticallyWaitsToMinimizeStalling = false
            player.play()
        } else {
            let identifier = state.playlist.source.identifier
            let fileName = state.song.fileName
            let downloadUrl = "https://archive.org/download/\(identifier)/\(fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
            
            item = AVPlayerItem(url: URL(string: downloadUrl)!)
            player = AVPlayer(playerItem: item)
            player.automaticallyWaitsToMinimizeStalling = false
            player.play()
        }
        
        setupNowPlaying(state.song)
        
        player.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1, preferredTimescale: 1), queue: .main) { [weak self] time in
            guard let self = self, time.isValid, !self.viewIsSeeking else { return }
            self.updateSeeker()
            self.setupNowPlaying(state.song)
        }
        
        showPopup = true
        percentFinished = 0
        
        let nc = NotificationCenter.default
        
        if let current = songEndedObserver {
            current.cancel()
        }
        songEndedObserver = nc.publisher(for: .AVPlayerItemDidPlayToEndTime).sink { [weak self] _ in
            self?.dispatch(action: .nextTrack)
        }
        
        if let current = interruptionObserver {
            current.cancel()
        }
        interruptionObserver = nc.publisher(for: AVAudioSession.interruptionNotification).sink { [weak self] _ in
            self?.dispatch(action: .pause)
        }
        
        cacheNextSong()
    }
    
    var nextSongTask: DownloadTask_?
    func cacheNextSong() {
        guard let state = state else { return }
        
        let nextIndex = state.songIndex() + 1
        guard nextIndex < state.playlist.songs.count else { return }
        let nextSong = state.playlist.songs[nextIndex]
        
        let downloadUrl = "https://archive.org/download/\(state.playlist.source.identifier)/\(nextSong.fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
        if let task = nextSongTask {
            task.cancel()
        }
        
        guard storage.getLocalSongUrl(recording: state.playlist.source, song: nextSong) == nil else { return }
        
        nextSongTask = DownloadTask_(url: URL(string: downloadUrl)!) { url in
            print("got url")
            self.storage.moveDownloadToCache(url, state.playlist.source.identifier, nextSong)
        }
        nextSongTask?.resume()
    }
    
    func updateSeeker() {
        guard let item = item else { return }
        
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
        if let item = item {
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
        if let item = item, item.duration.isNumeric {
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
                self.dispatch(action: .resume)
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.pauseCommand.addTarget { [weak self] _ in
            guard let self = self, let state = self.state else { return .noActionableNowPlayingItem }
            
            if state.playing {
                self.dispatch(action: .pause)
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.nextTrackCommand.addTarget { [weak self] _ in
            guard let self = self else { return .noActionableNowPlayingItem }
            self.dispatch(action: .nextTrack)
            return .success
        }
        
        commandCenter.previousTrackCommand.addTarget { [weak self] _ in
            guard let self = self else { return .noActionableNowPlayingItem }
            self.dispatch(action: .previousTrack)
            return .success
        }
        
        commandCenter.changePlaybackPositionCommand.addTarget { [weak self] (event) -> MPRemoteCommandHandlerStatus in
            guard let self = self else { return .noActionableNowPlayingItem }
            
            if let event = event as? MPChangePlaybackPositionCommandEvent {
                self.player.seek(to: CMTime(seconds: event.positionTime, preferredTimescale: 100))
                return .success
            }
            return .commandFailed
        }
    }
    
    func setupNowPlaying(_ song: Song) {
        let info = Info(
            title: song.title,
            currentTime: player.currentTime().seconds,
            duration: player.currentItem?.duration.seconds ?? 0.0,
            rate: player.rate,
            artist: state!.playlist.band.name,
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
