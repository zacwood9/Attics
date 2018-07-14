//
//  NowPlayingController.swift
//  Attics
//
//  Created by Zachary Wood on 6/16/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import FontAwesome
import MediaPlayer

class NowPlayingController: UIViewController, UITableViewDelegate {
    
    private lazy var showDateLabel: UILabel = {
        let label = UILabel()
        label.translatesAutoresizingMaskIntoConstraints = false
        label.font = UIFont.preferredFont(forTextStyle: .largeTitle, withSymbolicTraits: .traitBold)
        return label
    }()
    
    private lazy var venueLabel: UILabel = {
        let label = UILabel()
        label.translatesAutoresizingMaskIntoConstraints = false
        label.font = UIFont.preferredFont(forTextStyle: .title2)
        return label
    }()
    
    private lazy var infoButton: UIButton = {
        let button = UIButton()
        button.translatesAutoresizingMaskIntoConstraints = false
        button.titleLabel?.font = UIFont.fontAwesome(ofSize: 25)
        button.setTitle(String.fontAwesomeIcon(name: .infoCircle), for: .normal)
        button.setTitleColor(.blue, for: .normal)
        return button
    }()
    
    private lazy var tableView: UITableView = {
        let tableView = UITableView()
        tableView.dataSource = dataSource
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: "Song Cell")
        tableView.translatesAutoresizingMaskIntoConstraints = false
        tableView.delegate = self
        return tableView
    }()
    
    private var timeSlider: UISlider
    
    private lazy var currentTimeLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont.preferredFont(forTextStyle: .body)
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()
    
    private lazy var durationLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont.preferredFont(forTextStyle: .body)
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()
    
    private var playPauseButton: UIButton
    private var advanceButton: UIButton
    
    private var playPauseBarButton: UIBarButtonItem
    private var seperator: UIBarButtonItem
    private var advanceBarButton: UIBarButtonItem
    
    var dataSource: SongsDataSource
    private var selectRowCallback: (IndexPath) -> ()
    private var musicPlayer: MusicPlayer
    
    private var isSliderDown = false
    
    @objc func playerWasPaused() {
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .play, target: musicPlayer, action: #selector(musicPlayer.resume))
        playPauseButton.removeTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.resume), for: .touchUpInside)
        playPauseButton.setTitle(String.fontAwesomeIcon(name: .play), for: .normal)
        
        setBarButtons()
    }
    
    @objc func playerDidPlay(notification: Notification) {
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))
        
        playPauseButton.removeTarget(musicPlayer, action: #selector(musicPlayer.resume), for: .touchUpInside)
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        playPauseButton.setTitle(String.fontAwesomeIcon(name: .pause), for: .normal)
        
        setInfo(for: musicPlayer.currentSong)
        tableView.selectRow(at: IndexPath(row: musicPlayer.currentSongIndex, section: 0), animated: false, scrollPosition: .none)
        setBarButtons()
    }
    
    private func setInfo(for song: Song) {
        popupItem.title = song.title
        popupItem.subtitle = song.album
        
        showDateLabel.text = musicPlayer.playerState.show.date
        venueLabel.text = musicPlayer.playerState.show.venue
    }
    
    private func setBarButtons() {
        popupItem.rightBarButtonItems = [playPauseBarButton, seperator, advanceBarButton]
    }
    
    @objc func silderDidChange(sender: UISlider) {
        let timeAtSlider = musicPlayer.currentDurationInSeconds * Double(sender.value)
        currentTimeLabel.text = timeAtSlider.timeString
        durationLabel.text = (musicPlayer.currentDurationInSeconds - timeAtSlider).timeString
    }
    
    @objc func updateSlider(notification: Notification) {
        if let percentage = notification.object as? Double, isSliderDown == false {
            timeSlider.setValue(Float(percentage), animated: true)
            currentTimeLabel.text = musicPlayer.currentTimeInSeconds.timeString
            durationLabel.text = (musicPlayer.currentDurationInSeconds - musicPlayer.currentTimeInSeconds).timeString
        }
    }
    
    @objc func sliderWasPressed() {
        isSliderDown = true
    }
    
    @objc func sliderWasUnpressed() {
        isSliderDown = false
        musicPlayer.seek(percentage: Double(exactly: timeSlider.value)!)
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        musicPlayer.play(index: indexPath.row, in: musicPlayer.songList)
        self.selectRowCallback(indexPath)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        configureViews()
        setupObservers()
    }
    
    
    private func configureViews() {
        view.layer.cornerRadius = 16
        view.backgroundColor = .white
        
        view.addSubview(venueLabel)
        view.addSubview(showDateLabel)
        view.addSubview(infoButton)
        view.addSubview(tableView)
        view.addSubview(timeSlider)
        view.addSubview(playPauseButton)
        view.addSubview(advanceButton)
        view.addSubview(currentTimeLabel)
        view.addSubview(durationLabel)
        
        NSLayoutConstraint.activate([
            showDateLabel.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor, constant: 32),
            showDateLabel.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor, constant: 16),
            
            infoButton.centerYAnchor.constraint(equalTo: showDateLabel.centerYAnchor, constant: 0),
            infoButton.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor, constant: -16),
            
            venueLabel.topAnchor.constraint(equalTo: showDateLabel.bottomAnchor, constant: 4),
            venueLabel.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor, constant: 16),
            
            tableView.topAnchor.constraint(equalTo: venueLabel.bottomAnchor, constant: 16),
            tableView.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor, constant: 0),
            tableView.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor, constant: 0),
            
            timeSlider.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor, constant: -125),
            timeSlider.topAnchor.constraint(equalTo: tableView.bottomAnchor, constant: 8),
            timeSlider.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor, constant: 32),
            timeSlider.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor, constant: -32),
            
            currentTimeLabel.leftAnchor.constraint(equalTo: timeSlider.leftAnchor),
            currentTimeLabel.topAnchor.constraint(equalTo: timeSlider.bottomAnchor, constant: 2),
            
            durationLabel.rightAnchor.constraint(equalTo: timeSlider.rightAnchor),
            durationLabel.topAnchor.constraint(equalTo: timeSlider.bottomAnchor, constant: 2),
            
            playPauseButton.topAnchor.constraint(equalTo: currentTimeLabel.bottomAnchor, constant: 16),
            playPauseButton.bottomAnchor.constraint(lessThanOrEqualTo: view.safeAreaLayoutGuide.bottomAnchor, constant: -8),
            playPauseButton.centerXAnchor.constraint(equalTo: view.centerXAnchor, constant: 0),
            
            advanceButton.centerYAnchor.constraint(equalTo: playPauseButton.centerYAnchor),
            advanceButton.leftAnchor.constraint(equalTo: playPauseButton.rightAnchor, constant: 58)
            ])
    }
    
    private func setupObservers() {
        NotificationCenter.default.addObserver(self, selector: #selector(playerDidPlay(notification:)), name: .MusicPlayerDidPlay, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(playerWasPaused), name: .MusicPlayerDidPause, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(updateSlider(notification:)), name: .MusicPlayerDidChangeTime, object: nil)
        
        timeSlider.addTarget(self, action: #selector(silderDidChange(sender:)), for: .valueChanged)
        timeSlider.addTarget(self, action: #selector(sliderWasPressed), for: .touchDown)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchUpInside)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchUpOutside)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchCancel)
        
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        advanceButton.addTarget(musicPlayer, action: #selector(musicPlayer.playNextTrack), for: .touchUpInside)
    }
    
    init(selectedIndexPath: IndexPath, selectRowCallback: @escaping (IndexPath) -> (), musicPlayer: MusicPlayer = MusicPlayer.instance) {
        self.musicPlayer = musicPlayer
        self.dataSource = SongsDataSource(songs: self.musicPlayer.songList)
        self.selectRowCallback = selectRowCallback
        
        playPauseButton = UIButton(type: .system)
        playPauseButton.titleLabel?.font = UIFont.fontAwesome(ofSize: 40)
        playPauseButton.setTitle(String.fontAwesomeIcon(name: .pause), for: .normal)
        playPauseButton.translatesAutoresizingMaskIntoConstraints = false
        
        advanceButton = UIButton(type: .system)
        advanceButton.titleLabel?.font = UIFont.fontAwesome(ofSize: 40)
        advanceButton.setTitle(String.fontAwesomeIcon(name: .forward), for: .normal)
        advanceButton.translatesAutoresizingMaskIntoConstraints = false
        
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))
        advanceBarButton = UIBarButtonItem(barButtonSystemItem: .fastForward, target: musicPlayer, action: #selector(musicPlayer.playNextTrack))
        seperator =  {
            let button = UIBarButtonItem(barButtonSystemItem: .fixedSpace, target: nil, action: nil)
            button.width = 16
            return button
        }()
        
        timeSlider = UISlider()
        timeSlider.isContinuous = true
        timeSlider.translatesAutoresizingMaskIntoConstraints = false
        
        super.init(nibName: nil, bundle: nil)
        
        setInfo(for: musicPlayer.currentSong)
        setBarButtons()
        
        tableView.selectRow(at: selectedIndexPath, animated: false, scrollPosition: .top)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
}
