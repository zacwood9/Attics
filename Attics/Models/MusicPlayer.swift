//
//  MusicPlayer.swift
//  Attics
//
//  Created by Zachary Wood on 6/16/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import AVKit

enum PlayerStatus {
    case playing
    case paused
}

protocol MusicPlayerDelegate: class {
    func playerWasPaused()
    func playerWasPlayed()
}

class MusicPlayer {
    static var instance = MusicPlayer()
    
    var status: PlayerStatus {
        return currentPlayer.timeControlStatus == .playing ? .playing : .paused
    }
    
    private var currentPlayer = AVPlayer(playerItem: nil)
    
    func play(song: Song?) {
        if let song = song {
            currentPlayer = AVPlayer(url: song.downloadURL)
            currentPlayer.automaticallyWaitsToMinimizeStalling = false
        }

        currentPlayer.play()
    }
    
    func pause() {
        currentPlayer.pause()
    }
}
