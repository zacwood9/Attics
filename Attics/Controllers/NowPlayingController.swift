//
//  NowPlayingController.swift
//  Attics
//
//  Created by Zachary Wood on 6/16/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import MediaPlayer

class NowPlayingController: UIViewController {
    
    private var playPauseButton: UIBarButtonItem
    private var advanceButton: UIBarButtonItem
    private var seperator: UIBarButtonItem
    
    private var timeSlider: UISlider
    
    private var musicPlayer: MusicPlayer
    
    @objc func playerWasPaused() {
        playPauseButton = UIBarButtonItem(barButtonSystemItem: .play, target: musicPlayer, action: #selector(musicPlayer.resume))
        setBarButtons()
    }
    
    @objc func playerDidPlay(notification: Notification) {
        playPauseButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))
        setInfo(for: musicPlayer.currentSong)
        setBarButtons()
    }
    
    private func setInfo(for song: Song) {
        popupItem.title = song.title
        popupItem.subtitle = "\(song.source.show.date) - \(song.source.show.venue)"
    }
    
    private func setBarButtons() {
        popupItem.rightBarButtonItems = [playPauseButton, seperator, advanceButton]
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .white
        
        view.addSubview(timeSlider)
        NSLayoutConstraint.activate([
            timeSlider.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor, constant: -64),
            timeSlider.bottomAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor, constant: 0),
            timeSlider.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor, constant: 32),
            timeSlider.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor, constant: -32)
            ])
    }
    
    init(musicPlayer: MusicPlayer = MusicPlayer.instance) {
        self.musicPlayer = musicPlayer
        playPauseButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))
        advanceButton = UIBarButtonItem(barButtonSystemItem: .fastForward, target: musicPlayer, action: #selector(musicPlayer.playNextTrack))
        seperator =  {
            let button = UIBarButtonItem(barButtonSystemItem: .fixedSpace, target: nil, action: nil)
            button.width = 16
            return button
        }()
        timeSlider = UISlider()
        timeSlider.translatesAutoresizingMaskIntoConstraints = false
        
        super.init(nibName: nil, bundle: nil)
        
        setInfo(for: musicPlayer.currentSong)
        setBarButtons()
        
        NotificationCenter.default.addObserver(self, selector: #selector(playerDidPlay(notification:)), name: .MusicPlayerDidPlay, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(playerWasPaused), name: .MusicPlayerDidPause, object: nil)
    }
    
    
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
}
