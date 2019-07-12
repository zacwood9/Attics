//
//  MusicPlayer.swift
//  Attics
//
//  Created by Zachary Wood on 6/16/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import AVKit
import MediaPlayer

enum PlayerStatus {
    case playing
    case paused
}

struct PlayerState {
    let show: Show
    let source: Source
    let songs: [Song]
    let status: PlayerStatus
}

class MusicPlayer {
    static var instance = MusicPlayer()

    var status: PlayerStatus {
        return currentPlayer.timeControlStatus == .playing ? .playing : .paused
    }
    
    private var currentPlayer = AVPlayer(playerItem: nil)
    var playerState: PlayerState!
    var songList: [Song] = []
    var currentSongIndex = 0
    var currentSong: Song {
        return songList[currentSongIndex]
    }
    var currentTimeInSeconds: Double {
        return currentPlayer.currentTime().seconds
    }
    var currentDurationInSeconds: Double {
        guard let currentItem = currentPlayer.currentItem, currentItem.duration.isNumeric else { return 0.0 }
        return currentItem.duration.seconds
    }
    private var percentageSongFinished: Double {
        guard let currentItem = currentPlayer.currentItem else { return 0.0 }
        
        return currentPlayer.currentTime().seconds / currentItem.duration.seconds
    }

    init() {
        setupRemoteControls()
        NotificationCenter.default.addObserver(self, selector: #selector(playNextTrack), name: .AVPlayerItemDidPlayToEndTime, object: nil)
        try? AVAudioSession.sharedInstance().setActive(true, options: AVAudioSession.SetActiveOptions.notifyOthersOnDeactivation)
    }
    
    deinit {
        try? AVAudioSession.sharedInstance().setActive(false, options: AVAudioSession.SetActiveOptions.notifyOthersOnDeactivation)
    }
    
    func play(index: Int, in songList: [Song]) {
        self.currentSongIndex = index
        self.songList = songList
        
        let song = playerState.songs[index]
        let asset = AVURLAsset(url: song.downloadUrl)
        let item = AVPlayerItem(asset: asset)
        currentPlayer = AVPlayer(playerItem: item)
        currentPlayer.automaticallyWaitsToMinimizeStalling = false
            
        setupNowPlaying(song)
        
        currentPlayer.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1.0, preferredTimescale: 2),
                                              queue: DispatchQueue.main) { [unowned self] _ in
            self.setupNowPlaying(self.currentSong)
            NotificationCenter.default.post(name: .MusicPlayerDidChangeTime, object: self.percentageSongFinished)
        }
        
        currentPlayer.play()
        NotificationCenter.default.post(name: .MusicPlayerDidPlay, object: song)
        UIDevice.vibrate()
    }
    
    @objc func pause() {
        currentPlayer.pause()
        NotificationCenter.default.post(name: .MusicPlayerDidPause, object: nil)
        UIDevice.vibrate()
    }
    
    @objc func resume() {
        currentPlayer.play()
        NotificationCenter.default.post(name: .MusicPlayerDidPlay, object: nil)
        UIDevice.vibrate()
    }
    
    @objc func playNextTrack() {
        if currentSongIndex + 1 < songList.count {
            currentSongIndex += 1
            play(index: currentSongIndex, in: songList)
        }
        UIDevice.vibrate()
    }
    
    @objc func playPreviousTrack() {
        if currentPlayer.currentTime().seconds > 2 {
            currentPlayer.seek(to: CMTime(seconds: 0, preferredTimescale: 100))
        } else {
            if (currentSongIndex - 1 >= 0) {
                currentSongIndex -= 1
                play(index: currentSongIndex, in: songList)
            } else {
                currentPlayer.seek(to: CMTime(seconds: 0, preferredTimescale: 100))
            }
        }
        UIDevice.vibrate()
    }
    
    @objc func seek(to time: Double) {
        currentPlayer.seek(to: CMTime(seconds: time, preferredTimescale: 100))
    }
    
    @objc func seek(percentage: Double) {
        if let totalSeconds = currentPlayer.currentItem?.duration.seconds {
            seek(to: totalSeconds * percentage)
        }
    }
    
    private func setupRemoteControls() {
        let commandCenter = MPRemoteCommandCenter.shared()
        
        commandCenter.playCommand.addTarget { [unowned self] _ in
            if self.status == .paused {
                self.resume()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.pauseCommand.addTarget { [unowned self] _ in
            if self.status == .playing {
                self.pause()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.nextTrackCommand.addTarget { [unowned self] _ in
            self.playNextTrack()
            return .success
        }
        
        commandCenter.previousTrackCommand.addTarget { [unowned self] _ in
            self.playPreviousTrack()
            return .success
        }
        
        commandCenter.changePlaybackPositionCommand.addTarget { [unowned self] (event) -> MPRemoteCommandHandlerStatus in
            if let event = event as? MPChangePlaybackPositionCommandEvent {
                self.seek(to: event.positionTime)
                return .success
            }
            return .commandFailed
        }
    }
    
    func setupNowPlaying(_ song: Song) {
        let info = Info(title: song.title,
                        currentTime: currentPlayer.currentTime().seconds,
                        duration: currentPlayer.currentItem?.duration.seconds ?? 0.0,
                        rate: currentPlayer.rate,
                        artist: "Grateful Dead",
                        album: song.album ?? "Grateful Dead")
        
        MPNowPlayingInfoCenter.default().nowPlayingInfo = info.toNowPlayingInfo()
    }
}

extension Notification.Name {
    public static let MusicPlayerDidPlay = Notification.Name("MusicPlayerDidPlay")
    public static let MusicPlayerDidPause = Notification.Name("MusicPlayerDidPause")
    public static let MusicPlayerDidChangeTime = Notification.Name("MusicPlayerDidChangeTime")
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
