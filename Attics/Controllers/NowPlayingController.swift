//
//  NowPlayingController.swift
//  Attics
//
//  Created by Zachary Wood on 6/16/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class NowPlayingController: UIViewController {
    
    private var playPauseButton: UIBarButtonItem!
    private var musicPlayer: MusicPlayer
    
    @objc func togglePlayPause() {
        switch musicPlayer.status {
        case .paused:
            musicPlayer.play(song: nil)
            playPauseButton = UIBarButtonItem(barButtonSystemItem: .pause, target: self, action: #selector(togglePlayPause))
        case .playing:
            musicPlayer.pause()
            playPauseButton = UIBarButtonItem(barButtonSystemItem: .play, target: self, action: #selector(togglePlayPause))
        }
        
        popupItem.rightBarButtonItems = [playPauseButton]
    }
    
    init(songs: [Song], selected: Int, musicPlayer: MusicPlayer = MusicPlayer.instance) {
        self.musicPlayer = musicPlayer
        
        super.init(nibName: nil, bundle: nil)
        
        playPauseButton = UIBarButtonItem(barButtonSystemItem: .pause, target: self, action: #selector(togglePlayPause))
        popupItem.rightBarButtonItems = [playPauseButton]
        
        popupItem.title = songs[selected].title
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
}
