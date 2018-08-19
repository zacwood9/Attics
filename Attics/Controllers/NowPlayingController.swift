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
import FontAwesome

class NowPlayingController: UIViewController, UITableViewDelegate {
    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var dateLabel: UILabel!
    @IBOutlet weak var venueLabel: UILabel!
    @IBOutlet weak var locationLabel: UILabel!
//    @IBOutlet weak var downloadButton: UIButton!
    @IBOutlet weak var infoButton: UIButton!
    @IBOutlet weak var timeSlider: UISlider!
    @IBOutlet weak var currentTimeLabel: UILabel!
    @IBOutlet weak var durationLabel: UILabel!
    @IBOutlet weak var playPauseButton: UIButton!
    @IBOutlet weak var advanceButton: UIButton!
    
    var dataSource: SongsDataSource!
    private var playPauseBarButton: UIBarButtonItem!
    private var seperator: UIBarButtonItem!
    private var advanceBarButton: UIBarButtonItem!
    
    private var isSliderDown = false

    var musicPlayer = MusicPlayer.instance
    
    var onMoreInfoTapped: (Source, UIView) -> () = { _,_ in }
    
    override func viewDidLoad() {
        setupInitialViews()
        setBarButtons()
        setupObservers()
    }
    
    private func setupInitialViews() {
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))
        advanceBarButton = UIBarButtonItem(barButtonSystemItem: .fastForward, target: musicPlayer, action: #selector(musicPlayer.playNextTrack))
        seperator =  {
            let button = UIBarButtonItem(barButtonSystemItem: .fixedSpace, target: nil, action: nil)
            button.width = 16
            return button
        }()
        
        tableView.dataSource = dataSource
        tableView.delegate = self
        tableView.selectRow(at: IndexPath(row: musicPlayer.currentSongIndex, section: 0), animated: false, scrollPosition: .none)
        
        dateLabel.font = UIFont.preferredFont(forTextStyle: .largeTitle, withSymbolicTraits: .traitBold)
        
        playPauseButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .pause), ofSize: 46)
        advanceButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .forward), ofSize: 46)
//        downloadButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .cloudDownloadAlt), ofSize: 26)
        infoButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .ellipsisH), ofSize: 26)
        
        configureLabels()
    }
    
    
    private func setupObservers() {
        
        NotificationCenter.default.addObserver(self, selector: #selector(updateSlider(notification:)), name: .MusicPlayerDidChangeTime, object: nil)
        
        timeSlider.addTarget(self, action: #selector(silderDidChange(sender:)), for: .valueChanged)
        timeSlider.addTarget(self, action: #selector(sliderWasPressed), for: .touchDown)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchUpInside)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchUpOutside)
        timeSlider.addTarget(self, action: #selector(sliderWasUnpressed), for: .touchCancel)
        
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        advanceButton.addTarget(musicPlayer, action: #selector(musicPlayer.playNextTrack), for: .touchUpInside)
        infoButton.addTarget(self, action: #selector(showMoreInfoAlert(_:)), for: .touchUpInside)
        
        NotificationCenter.default.addObserver(self, selector: #selector(playerDidPlay(notification:)), name: .MusicPlayerDidPlay, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(playerWasPaused), name: .MusicPlayerDidPause, object: nil)
    }

    
    private func setBarButtons() {
        popupItem.rightBarButtonItems = [playPauseBarButton, seperator, advanceBarButton]
    }
    
    func configureLabels() {
        let show = musicPlayer.currentSong.source.show
        dateLabel.text = show.date
        venueLabel.text = show.venue
        locationLabel.text = "\(show.city), \(show.state)"
        
        popupItem.title = musicPlayer.currentSong.title
        popupItem.subtitle = musicPlayer.currentSong.album
    }
    
    @objc func showMoreInfoAlert(_ sender: UIView) {
        onMoreInfoTapped(musicPlayer.currentSong.source, sender);
    }
    
    @objc func playerWasPaused() {
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .play, target: musicPlayer, action: #selector(musicPlayer.resume))
        
        playPauseButton.removeTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.resume), for: .touchUpInside)
        playPauseButton.setTitle(String.fontAwesomeIcon(name: .play), for: .normal)

        setBarButtons()
        configureLabels()
    }

    @objc func playerDidPlay(notification: Notification) {
        playPauseBarButton = UIBarButtonItem(barButtonSystemItem: .pause, target: musicPlayer, action: #selector(musicPlayer.pause))

        playPauseButton.removeTarget(musicPlayer, action: #selector(musicPlayer.resume), for: .touchUpInside)
        playPauseButton.addTarget(musicPlayer, action: #selector(musicPlayer.pause), for: .touchUpInside)
        playPauseButton.setTitle(String.fontAwesomeIcon(name: .pause), for: .normal)
        
        tableView.selectRow(at: IndexPath(row: musicPlayer.currentSongIndex, section: 0), animated: false, scrollPosition: .none)
        setBarButtons()
        configureLabels()
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
    }

    deinit {
        NotificationCenter.default.removeObserver(self)
    }
}
