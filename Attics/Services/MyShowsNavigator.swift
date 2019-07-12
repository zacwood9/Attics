//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import CoreData

final class MyShowsNavigator: NSObject, UINavigationControllerDelegate {
    let navigationController: UINavigationController
    let dataStore = NetworkDataStore()
    let cache = CacheDataStore(context: (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext)
    let favoriteStore = UDFavoritesStore()
    
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    
    var isStartingUp = true
    var restoreDepth = 0
    
    var state = AppState()
    
    override init() {
        let sourcesVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.myShows) as! MyShowsViewController
        navigationController = UINavigationController(rootViewController: sourcesVC)
        super.init()
        sourcesVC.dataStore = favoriteStore
        sourcesVC.onSourceTap = pushSongsController
        sourcesVC.sources = favoriteStore.loadFavorites()
        
        if let source = state.source {
            pushSongsController(source: source)
        }
        
        configureNavigationController()
    }
    
    func pushSongsController(source: Source) {
        let songsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.songs) as! SongsViewController
        songsVC.source = source
        songsVC.dataStore = dataStore
        songsVC.onSongTapped = presentNowPlayingController
        songsVC.onMoreInfoTapped = presentSourceInfoAlert
        songsVC.onFavoriteTapped = { [weak self] s in
            guard let self = self else { return }
            if self.favoriteStore.isFavorite(source: s) {
                self.favoriteStore.removeFavorite(source: s)
            } else {
                self.favoriteStore.saveFavorite(source: s)
            }
            UIDevice.vibrate(style: .medium)
        }
        songsVC.isFavorite = { [weak self] in
            return self?.favoriteStore.isFavorite(source: source) ?? false
        }
        
        state.source = source
        navigationController.pushViewController(songsVC, animated: true)
    }
    
    func presentSourceInfoAlert( _ source: Source, _ sender: UIView) {
        let alertVC = UIAlertController(title: nil, message: nil, preferredStyle: .actionSheet)
        let currentVC = navigationController.viewControllers.last
        
        alertVC.addAction(UIAlertAction(title: "View Source Info", style: .default, handler: { _ in
            currentVC?.presentSafariViewController(at: URL(string: "https://archive.org/details/\(source.identifier)")!)
        }))
        
        alertVC.addAction(UIAlertAction(title: "Cancel", style: .cancel, handler: nil))
        
        if let popoverController = alertVC.popoverPresentationController { // handle iPads
            popoverController.sourceView = sender
        }
        
        currentVC?.present(alertVC, animated: true, completion: nil)
    }
    
    func presentNowPlayingController(playing index: Int, in songs: [Song]) {
        let song = songs[index]
        
        MusicPlayer.instance.playerState = PlayerState(show: song.source.show, source: song.source, songs: songs, status: .paused)
        MusicPlayer.instance.play(index: index, in: songs)
        
        let popupVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.nowPlaying) as! NowPlayingController
        let dataSource = SongsDataSource(songs: songs)
        popupVC.dataSource = dataSource
        popupVC.onMoreInfoTapped = presentSourceInfoAlert
        popupVC.onFavoriteTapped = { [weak self] s in
            guard let self = self else { return }
            if self.favoriteStore.isFavorite(source: s) {
                self.favoriteStore.removeFavorite(source: s)
            } else {
                self.favoriteStore.saveFavorite(source: s)
            }
            UIDevice.vibrate(style: .medium)
        }
        popupVC.isFavorite = { [weak self] in
            return self?.favoriteStore.isFavorite(source: song.source) ?? false
        }
        navigationController.tabBarController?.presentPopupBar(withContentViewController: popupVC, animated: true, completion: nil)
    }
    
    private func configureNavigationController() {
        navigationController.navigationBar.prefersLargeTitles = true
        navigationController.navigationBar.barTintColor = #colorLiteral(red: 0.1997258663, green: 0.2665995359, blue: 0.5491077304, alpha: 1)
        navigationController.navigationBar.isTranslucent = false
        navigationController.navigationBar.largeTitleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.titleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.tintColor = .white
        navigationController.tabBarItem = UITabBarItem(title: "My Shows", image: UIImage.fontAwesomeIcon(name: .heart, style: .solid, textColor: UIColor.black, size: CGSize(width: 30, height: 30)), tag: 1)
        navigationController.delegate = self
    }
    
    func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated: Bool) {
        if viewController is MyShowsViewController {
            state.source = nil
        } else if let vc = viewController as? SongsViewController {
            state.source = vc.source
        }
    }
}
