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
}

typealias playerChangeHandler = (PlayerStatus, Song, Double, Double) -> ()

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
    
    func play(index: Int, in songList: [Song]) {
        self.currentSongIndex = index
        self.songList = songList
        
        let song = playerState.songs[index]
        let asset = AVURLAsset(url: downloadUrl(source: playerState.source, song: song))
        let item = AVPlayerItem(asset: asset)
        currentPlayer = AVPlayer(playerItem: item)
        currentPlayer.automaticallyWaitsToMinimizeStalling = false
            
        setupNowPlaying(song)
        
        currentPlayer.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1.0, preferredTimescale: 2), queue: DispatchQueue.main) { [unowned self] _ in
            self.setupNowPlaying(self.currentSong)
            NotificationCenter.default.post(name: .MusicPlayerDidChangeTime, object: self.percentageSongFinished)
        }
        
        currentPlayer.play()
        NotificationCenter.default.post(name: .MusicPlayerDidPlay, object: song)
    }
    
    @objc func pause() {
        currentPlayer.pause()
        NotificationCenter.default.post(name: .MusicPlayerDidPause, object: nil)
    }
    
    @objc func resume() {
        currentPlayer.play()
        NotificationCenter.default.post(name: .MusicPlayerDidPlay, object: nil)
    }
    
    @objc func playNextTrack() {
        if (currentSongIndex + 1 < songList.count) {
            currentSongIndex += 1
            play(index: currentSongIndex, in: songList)
        }

    }
    
    @objc func playPreviousTrack() {
        if (currentSongIndex - 1 >= 0) {
            currentSongIndex -= 1
            play(index: currentSongIndex, in: songList)
        }
    }
    
    @objc func seek(to time: Double) {
        currentPlayer.seek(to: CMTime(seconds: time, preferredTimescale: 100))
    }
    
    @objc func seek(percentage: Double) {
        if let totalSeconds = currentPlayer.currentItem?.duration.seconds {
            seek(to: totalSeconds * percentage)
        }
    }
    
    func setupRemoteControls() {
        let commandCenter = MPRemoteCommandCenter.shared()
        
        commandCenter.playCommand.addTarget { [unowned self] event in
            if self.status == .paused {
                self.resume()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.pauseCommand.addTarget { [unowned self] event in
            if self.status == .playing {
                self.pause()
                return .success
            }
            return .commandFailed
        }
        
        commandCenter.nextTrackCommand.addTarget { [unowned self] event in
            self.playNextTrack()
            return .success
        }
        
        commandCenter.previousTrackCommand.addTarget { [unowned self] event in
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
        var nowPlayingInfo = [String : Any]()
        nowPlayingInfo[MPMediaItemPropertyTitle] = song.title
        
        if let image = UIImage(named: "lockscreen") {
            nowPlayingInfo[MPMediaItemPropertyArtwork] =
                MPMediaItemArtwork(boundsSize: image.size) { size in
                    return image
            }
        }
        
        if let item = self.currentPlayer.currentItem {
            nowPlayingInfo[MPNowPlayingInfoPropertyElapsedPlaybackTime] = currentPlayer.currentTime().seconds
            nowPlayingInfo[MPMediaItemPropertyPlaybackDuration] = item.duration.seconds
            nowPlayingInfo[MPNowPlayingInfoPropertyPlaybackRate] = 1.0
        }

        nowPlayingInfo[MPNowPlayingInfoPropertyPlaybackRate] = self.currentPlayer.rate
        nowPlayingInfo[MPMediaItemPropertyArtist] = "Grateful Dead"
        nowPlayingInfo[MPMediaItemPropertyAlbumTitle] = "TODO - DATA"
        
        // Set the metadata
        MPNowPlayingInfoCenter.default().nowPlayingInfo = nowPlayingInfo
    }
    
    init() {
        setupRemoteControls()
        NotificationCenter.default.addObserver(self, selector: #selector(playNextTrack), name: .AVPlayerItemDidPlayToEndTime, object: nil)
    }
}

extension Notification.Name {
    public static let MusicPlayerDidPlay = Notification.Name("MusicPlayerDidPlay")
    public static let MusicPlayerDidPause = Notification.Name("MusicPlayerDidPause")
    public static let MusicPlayerDidChangeTime = Notification.Name("MusicPlayerDidChangeTime")
}
