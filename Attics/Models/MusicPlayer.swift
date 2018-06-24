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

class MusicPlayer {
    static var instance = MusicPlayer()
    
    var status: PlayerStatus {
        return currentPlayer.timeControlStatus == .playing ? .playing : .paused
    }
    
    private var currentPlayer = AVPlayer(playerItem: nil)
    
    var songList: [Song] = []
    var currentSongIndex = 0
    
    var currentSong: Song {
        return songList[currentSongIndex]
    }
    
    private var updateTimer: Timer?
    
    func play(index: Int, in songList: [Song]) {
        self.currentSongIndex = index
        self.songList = songList
        
        let song = songList[index]
        let asset = AVURLAsset(url: song.downloadURL)
        let item = AVPlayerItem(asset: asset)
        currentPlayer = AVPlayer(playerItem: item)
        currentPlayer.automaticallyWaitsToMinimizeStalling = false
            
        setupNowPlaying(song)
        
        currentPlayer.addPeriodicTimeObserver(forInterval: CMTime(seconds: 1.0, preferredTimescale: 2), queue: DispatchQueue.main) { [unowned self] _ in
            self.setupNowPlaying(self.currentSong)
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
                self.currentPlayer.seek(to: CMTime(seconds: event.positionTime, preferredTimescale: 100))
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
        nowPlayingInfo[MPMediaItemPropertyAlbumTitle] = "\(song.source.show.date) - \(song.source.show.venue)"
        
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
}
