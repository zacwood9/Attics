//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI
import Network
import Combine
import LNPopupController

// AtticsNavigator is the base class for the Browse and TopShows navigators.
class AtticsNavigator: NSObject, UINavigationControllerDelegate {
    let navigationController: UINavigationController
    let apiClient = APIClient()

    let storage: AppStorageManager
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    lazy var musicPlayer = App.shared.musicPlayer

    // Delegate downloads to parent since all navigators need to be in sync
    var startDownload: (Source, [Song]) -> () = { _,_ in }
    var removeDownload: (Source) -> () = { _ in }
    var isDownloading: (Source) -> Bool = { _ in false }
    var getProgress: (Source) -> ([Song:Double], Int) = { _ in ([:], 0) }

    var networkStatus: () -> NWPath.Status

    lazy var playBtn = UIBarButtonItem(barButtonSystemItem: .play, target: self, action: #selector(_play))
    lazy var pauseBtn = UIBarButtonItem(barButtonSystemItem: .pause, target: self, action: #selector(_pause))
    lazy var nextTrackBtn = UIBarButtonItem(barButtonSystemItem: .fastForward, target: self, action: #selector(_nextTrack))

    init(rootViewController: UIViewController, storage: AppStorageManager, networkStatus: @escaping () -> NWPath.Status) {
        self.storage = storage
        self.networkStatus = networkStatus
        navigationController = UINavigationController(rootViewController: rootViewController)
        super.init()

        configureNavigationController()
        restoreMusicPlayer()
    }

    // MARK: - Views

    func pushPerformancesController(band: Band, year: Year) {
        let showsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.shows) as! PerformancesViewController
        showsVC.band = band
        showsVC.year = year
        showsVC.api = apiClient
        showsVC.onShowTapped = pushSourcesController
        navigationController.pushViewController(showsVC, animated: true)
    }

    func pushSourcesController(band: Band, show: Show) {
        let sourcesVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.sources) as! RecordingsViewController
        sourcesVC.band = band
        sourcesVC.performance = show
        sourcesVC.api = apiClient
        sourcesVC.onSourceTap = pushRecordingController
        navigationController.pushViewController(sourcesVC, animated: true)
    }

    func pushRecordingController(band: Band, performance: Show, recording: Source) {
        let vm = RecordingViewModel(
            band: band,
            performance: performance,
            recording: recording,
            api: apiClient,
            storage: storage,
            songTapped: presentNowPlayingController
        )
        let vc = RecordingViewController(viewModel: vm, musicPlayer: musicPlayer)
        navigationController.pushViewController(vc, animated: true)
    }

    func pushRecordingController(_ stored: StoredRecording) {
        let vm = RecordingViewModel(
            band: stored.band,
            performance: stored.performance,
            recording: stored.recording,
            api: apiClient,
            storage: storage,
            songTapped: presentNowPlayingController
        )
        let vc = RecordingViewController(viewModel: vm, musicPlayer: musicPlayer)
        navigationController.pushViewController(vc, animated: true)
    }

    var nowPlayingVC: PlayerViewController!
    var musicPlayerStateCancellable: AnyCancellable?

    func presentNowPlayingController(stored: StoredRecording, song: Song) {
        let playlist = Playlist(band: stored.band, show: stored.performance, source: stored.recording, songs: stored.songs)
        musicPlayer.start(song, playlist)

        let vm = RecordingViewModel(
            band: stored.band,
            performance: stored.performance,
            recording: stored.recording,
            api: apiClient,
            storage: storage,
            songTapped: { [weak self] _,song in self?.musicPlayer.start(song, playlist) }
        )
        nowPlayingVC = PlayerViewController(viewModel: vm, musicPlayer: musicPlayer)

        nowPlayingVC.popupInteractionStyle = .scroll
        nowPlayingVC.popupItem.title = song.title
        nowPlayingVC.popupItem.subtitle = song.album
        nowPlayingVC.popupItem.trailingBarButtonItems = [pauseBtn, nextTrackBtn]

        musicPlayerStateCancellable = musicPlayer.$state.sink { [weak self] state in
            guard let self = self, let state = state else { return }

            if self.nowPlayingVC.popupItem.title != state.song.title {
                self.nowPlayingVC.popupItem.title = state.song.title
            }

            self.nowPlayingVC.popupItem.trailingBarButtonItems = [state.playing ? self.pauseBtn : self.playBtn, self.nextTrackBtn]
        }
        navigationController.tabBarController?.presentPopupBar(withContentViewController: nowPlayingVC, animated: true, completion: nil)
    }

    func restoreMusicPlayer() {
        guard var state = storage.musicPlayerState, musicPlayer.state == nil else { return }
        state.playing = false

        let vm = RecordingViewModel(
            band: state.playlist.band,
            performance: state.playlist.show,
            recording: state.playlist.source,
            api: apiClient,
            storage: storage,
            songTapped: { [weak self] _,song in self?.musicPlayer.start(song, state.playlist) }
        )
        nowPlayingVC = PlayerViewController(viewModel: vm, musicPlayer: musicPlayer)

        nowPlayingVC.popupInteractionStyle = .scroll
        nowPlayingVC.popupItem.title = state.song.title
        nowPlayingVC.popupItem.subtitle = state.song.album
        nowPlayingVC.popupItem.trailingBarButtonItems = [pauseBtn, nextTrackBtn]

        musicPlayerStateCancellable = musicPlayer.$state.sink { [weak self] state in
            guard let self = self, let state = state else { return }

            if self.nowPlayingVC.popupItem.title != state.song.title {
                self.nowPlayingVC.popupItem.title = state.song.title
            }

            self.nowPlayingVC.popupItem.trailingBarButtonItems = [state.playing ? self.pauseBtn : self.playBtn, self.nextTrackBtn]
        }


        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            self.navigationController.tabBarController?.presentPopupBar(withContentViewController: self.nowPlayingVC, animated: true, completion: nil)
            self.musicPlayer.restore(state)
        }
    }

    func presentBandsController() {
        let vm = BandsViewModel(
            apiClient: apiClient,
            storage: storage,
            onBandClick: { band in
                self.storage.band = Band(collection: band.collection, name: band.name, logoUrl: band.logoUrl)
                self.navigationController.viewControllers.last?.dismiss(animated: true, completion: nil)
            }
        )

        let bandsVC = BandsViewController(viewModel: vm)
        let currentVC = navigationController.viewControllers.last
        currentVC?.present(UINavigationController(rootViewController: bandsVC), animated: true, completion: nil)
    }

    func presentAlert(with actions: [UIAlertAction], from sender: UIView? = nil) {
        let alertVC = UIAlertController(title: nil, message: nil, preferredStyle: .actionSheet)
        actions.forEach { alertVC.addAction($0) }
        alertVC.addAction(UIAlertAction(title: "Cancel", style: .cancel, handler: nil))

        if sender != nil, let popoverController = alertVC.popoverPresentationController { // handle iPads
            popoverController.sourceView = sender
        }

        let currentVC = navigationController.viewControllers.last
        currentVC?.tabBarController?.present(alertVC, animated: true, completion: nil)
    }

    // MARK: - Utilities

    private func configureNavigationController() {
        navigationController.navigationBar.prefersLargeTitles = true
        navigationController.navigationBar.barTintColor = #colorLiteral(red: 0.1997258663, green: 0.2665995359, blue: 0.5491077304, alpha: 1)
        navigationController.navigationBar.isTranslucent = false
        navigationController.navigationBar.largeTitleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.titleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.tintColor = .white
        navigationController.delegate = self
    }

    @objc func _play() {
        musicPlayer.resume()
    }

    @objc func _pause() {
        musicPlayer.pause()
    }

    @objc func _nextTrack() {
        musicPlayer.nextTrack()
    }
}
