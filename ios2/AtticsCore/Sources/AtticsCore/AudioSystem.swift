//
//  File.swift
//
//
//  Created by Zachary Wood on 12/21/23.
//

import Foundation
import Combine
import MediaPlayer
import AVFoundation

public class AudioSystem: ObservableObject {
    
    let audioPlayer = AVQueuePlayer(playerItem: nil)
    
    @Published public internal(set) var playlist: Playlist?
    @Published public private(set) var isSeeking = false
    @Published public private(set) var isPlaying = false
    
    public var currentProgress: Double {
        guard let item = audioPlayer.currentItem, !item.duration.seconds.isNaN else { return 0 }
        let progress = item.currentTime().seconds
        let duration = item.duration.seconds
        return progress / duration
    }
    
    public var currentDuration: Double {
        guard let item = audioPlayer.currentItem, !item.duration.seconds.isNaN else { return 0 }
        return item.duration.seconds
    }
    
    public var secondsPlayed: Double {
        guard let playlist else { return 0 }
        return playlist.secondsPlayed
    }
    
    private var playerItems: [AVPlayerItem : Playlist.Track] = [:]
    private var interruptionObserver: AnyCancellable?
    private var itemStarted: AnyCancellable?
    
    public init() {        
        itemStarted = audioPlayer.publisher(for: \.currentItem, options: .new).sink { [weak self] item in
            guard let self = self,
                  let item = item,
                  let playlist = self.playlist,
                  let track = playerItems[item]
            else { return }
                        
            
            playlist.currentTrack = track
            self.setupNowPlaying()
        }
        
        audioPlayer.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1.0, preferredTimescale: 100), queue: .main) { [weak self] time in
            guard let self, let playlist else { return }
            playlist.secondsPlayed = time.seconds
        }
        
        interruptionObserver = NotificationCenter.default.publisher(for: AVAudioSession.interruptionNotification).sink { [weak self] _ in
            self?.pause()
        }
    }
    
    public func loadPlaylist(initialId: String, bandName: String, recordingId: String, tracks: [Playlist.Track]) {        
        let initialTrack = tracks.first { $0.id == initialId }
        guard let initialTrack else { return }
        
        playlist = Playlist(playlistId: recordingId, tracks: tracks, currentTrack: initialTrack)
    }
    
    public func play(id: String) {
        guard let playlist else { return }
        
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        setupRemoteControls()
        guard let trackIndex = playlist.tracks.firstIndex(where: { $0.id == id }) else { return }
        
        let tracksToQueue = playlist.tracks.dropFirst(trackIndex)
        let itemsToQueue = tracksToQueue.map { track in
            return AVPlayerItem(url: track.audioUrl)
        }
        
        audioPlayer.removeAllItems()
        audioPlayer.insert(itemsToQueue.first!, after: nil)
        
        playerItems = zip(tracksToQueue, itemsToQueue).reduce(into: [:]) { result, pair in
            let (track, item) = pair
            result[item] = track
        }
        
        audioPlayer.play()
        playlist.currentTrack = playlist.tracks[trackIndex]
        isPlaying = true
        setupNowPlaying()
    }
    
    public func resume() {
        audioPlayer.play()
        isPlaying = true
    }
    
    public func pause() {
        audioPlayer.pause()
        isPlaying = false
    }
    
    public func seek(toPercentage percentage: Double, callback: @escaping () -> Void) {
        guard let item = audioPlayer.currentItem, !item.duration.seconds.isNaN else { return }
        let seekSeconds = item.duration.seconds * percentage
        
        isSeeking = true
        audioPlayer.seek(to: CMTime(seconds: seekSeconds, preferredTimescale: 100)) { [weak self] _ in
            self?.isSeeking = false
            callback()
        }
    }
    
    internal func loadPlaylistData(_ playlistData: PlaylistData) {
        playlist = Playlist(playlistId: playlistData.id, tracks: playlistData.tracks, currentTrack: playlistData.currentTrack, secondsPlayed: playlistData.secondsPlayed)
        guard let playlist, let currentTrackIndex = playlist.currentTrackIndex else { return }
        
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        setupRemoteControls()
        
        let tracksToQueue = playlist.tracks.dropFirst(currentTrackIndex)
        let itemsToQueue = tracksToQueue.map { track in
            return AVPlayerItem(url: track.audioUrl)
        }
        
        audioPlayer.removeAllItems()
        audioPlayer.insert(itemsToQueue.first!, after: nil)
        
        playerItems = zip(tracksToQueue, itemsToQueue).reduce(into: [:]) { result, pair in
            let (track, item) = pair
            result[item] = track
        }
        
        audioPlayer.seek(to: CMTime(seconds: playlist.secondsPlayed, preferredTimescale: 100)) { [weak self] _ in
            self?.objectWillChange.send()
            self?.playlist?.objectWillChange.send()
        }
        
        playlist.currentTrack = playlist.tracks[currentTrackIndex]
        isPlaying = false
        setupNowPlaying()
    }
    
    public func nextTrack() {
        guard let playlist, let currentTrackIndex = playlist.currentTrackIndex else { return }
        
        if currentTrackIndex == playlist.tracks.count - 1 {
            play(id: playlist.tracks[0].id)
        } else {
            play(id: playlist.tracks[currentTrackIndex + 1].id)
        }
    }
    
    public func previousTrack() {
        guard let playlist, let currentTrackIndex = playlist.currentTrackIndex, currentTrackIndex > 0 else {
            return
        }
        
        play(id: playlist.tracks[currentTrackIndex - 1].id)
    }
    
    private func setupRemoteControls() {
        let commandCenter = MPRemoteCommandCenter.shared()
        
        commandCenter.togglePlayPauseCommand.addTarget { [weak self] _ in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            
            if self.isPlaying {
                self.pause()
            } else {
                self.resume()
            }
            
            return .success
        }
        
        commandCenter.playCommand.addTarget { [weak self] _ in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            
            if !self.isPlaying {
                self.resume()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.pauseCommand.addTarget { [weak self] _ in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            
            if self.isPlaying {
                self.pause()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.nextTrackCommand.addTarget { [weak self] _ in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            self.nextTrack()
            return .success
        }
        
        commandCenter.previousTrackCommand.addTarget { [weak self] _ in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            self.previousTrack()
            return .success
        }
        
        commandCenter.changePlaybackPositionCommand.addTarget { [weak self] (event) -> MPRemoteCommandHandlerStatus in
            guard let self, self.playlist != nil else { return .noActionableNowPlayingItem }
            
            if let event = event as? MPChangePlaybackPositionCommandEvent {
                self.audioPlayer.seek(to: CMTime(seconds: event.positionTime, preferredTimescale: 100))
                return .success
            }
            return .commandFailed
        }
    }
    
    func setupNowPlaying() {
        guard let playlist else { return }
        let track = playlist.currentTrack
        
        let info = Info(
            title: track.title,
            currentTime: audioPlayer.currentTime().seconds,
            duration: audioPlayer.currentItem?.duration.seconds ?? 0.0,
            rate: audioPlayer.rate,
            artist: "Grateful Dead",
            album: "1977-05-08: Barton Hall"
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
