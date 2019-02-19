//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

final class BrowseNavigator {
    let navigationController: UINavigationController
    let dataStore = NetworkDataStore()
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    
    func pushShowsController(year: Year) {
        let showsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.shows) as! ShowsViewController
        showsVC.year = year
        showsVC.dataStore = dataStore
        showsVC.onShowTapped = pushSourcesController
        navigationController.pushViewController(showsVC, animated: true)
    }
    
    func pushSourcesController(show: Show) {
        let sourcesVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.sources) as! SourcesViewController
        sourcesVC.show = show
        sourcesVC.dataStore = dataStore
        sourcesVC.onSourceTap = pushSongsController
        navigationController.pushViewController(sourcesVC, animated: true)
    }
    
    func pushSongsController(source: Source) {
        let songsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.songs) as! SongsViewController
        songsVC.source = source
        songsVC.dataStore = dataStore
        songsVC.onSongTapped = presentNowPlayingController
        songsVC.onMoreInfoTapped = presentSourceInfoAlert
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
        
        navigationController.tabBarController?.presentPopupBar(withContentViewController: popupVC, animated: true, completion: nil)
    }
    
    init() {
        let yearsViewController = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.yearsWithTopShows) as! YearsViewController
        
        navigationController = UINavigationController(rootViewController: yearsViewController)
        
        yearsViewController.dataStore = dataStore
        yearsViewController.onYearTapped = pushShowsController
        yearsViewController.onShowTapped = pushSourcesController
        
        configureNavigationController()
    }
    
    private func configureNavigationController() {
        navigationController.navigationBar.prefersLargeTitles = true
        navigationController.navigationBar.barTintColor = #colorLiteral(red: 0.1997258663, green: 0.2665995359, blue: 0.5491077304, alpha: 1)
        navigationController.navigationBar.isTranslucent = false
        navigationController.navigationBar.largeTitleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.titleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.tintColor = .white
        navigationController.tabBarItem = UITabBarItem(title: "Browse", image: UIImage.fontAwesomeIcon(name: .thList, style: .solid, textColor: UIColor.black, size: CGSize(width: 30, height: 30)), tag: 1)
    }
}
