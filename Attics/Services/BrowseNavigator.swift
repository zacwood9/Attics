//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import CoreData

final class BrowseNavigator: NSObject, UINavigationControllerDelegate {
    let navigationController: UINavigationController
    let dataStore = NetworkDataStore()
    let cache = CacheDataStore(context: (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext)
    let favoriteStore = CDFavoritesStore()
    
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    
    var state = AppState(id: "browse")
    var isStartingUp = true
    var restoreDepth = 0
    
    override init() {
        let yearsViewController = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.yearsWithTopShows) as! YearsViewController
        yearsViewController.context = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext
        
        navigationController = UINavigationController(rootViewController: yearsViewController)        
        super.init()
        
        yearsViewController.network = dataStore
        yearsViewController.onYearTapped = pushShowsController
        yearsViewController.onShowTapped = pushSourcesController
        
        configureNavigationController()
        
        if state.source != nil {
            restoreDepth = 3
        } else if state.show != nil {
            restoreDepth = 2
        } else if state.year != nil {
            restoreDepth = 1
        }
        
        if let year = state.year {
            pushShowsController(year: year)
        }
        if let show = state.show {
            pushSourcesController(show: show)
        }
        if let source = state.source {
            pushSongsController(source: source)
        }
    }
    
    func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated: Bool) {
        if viewController is YearsViewController {
            state.year = nil
            state.show = nil
            state.source = nil
        } else if let vc = viewController as? ShowsViewController {
            state.year = vc.year
            if restoreDepth == 1 { restoreDepth = 0; return }
            state.show = nil
            state.source = nil
        } else if let vc = viewController as? SourcesViewController {
            state.year = vc.show?.year
            state.show = vc.show
            if restoreDepth == 2 { restoreDepth = 0; return }
            state.source = nil
            
        } else if let vc = viewController as? SongsViewController {
            state.year = vc.source.show.year
            state.show = vc.source.show
            state.source = vc.source
        }
    }
    
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
    
    func onFavoriteTapped(_ s: Source) {
        if self.favoriteStore.isFavorite(source: s) {
            self.favoriteStore.removeFavorite(source: s)
        } else {
            self.favoriteStore.saveFavorite(source: s)
        }
        UIDevice.vibrate(style: .medium)
    }
    
    func isFavorite(_ s: Source) -> Bool {
        return self.favoriteStore.isFavorite(source: s)
    }
    
    func pushSongsController(source: Source) {
        let songsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.songs) as! SongsViewController
        songsVC.source = source
        songsVC.dataStore = dataStore
        songsVC.onSongTapped = presentNowPlayingController
        songsVC.onMoreInfoTapped = presentSourceInfoAlert
        songsVC.onFavoriteTapped = onFavoriteTapped
        songsVC.isFavorite = isFavorite
        
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
        popupVC.onFavoriteTapped = onFavoriteTapped
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
        navigationController.tabBarItem = UITabBarItem(title: "Browse", image: UIImage.fontAwesomeIcon(name: .thList, style: .solid, textColor: UIColor.black, size: CGSize(width: 30, height: 30)), tag: 1)
        navigationController.delegate = self
    }
}

struct AppState: Codable {
    let id: String
    
    var year: Year? = nil {
        didSet {
            if let year = year, let data = try? JSONEncoder().encode(year) {
                UserDefaults.standard.set(data, forKey: "\(id)-year")
            } else {
                UserDefaults.standard.removeObject(forKey: "\(id)-year")
            }
        }
    }
    
    var show: Show? = nil {
        didSet {
            if let show = show, let data = try? JSONEncoder().encode(show) {
                UserDefaults.standard.set(data, forKey: "\(id)-show")
            } else {
                UserDefaults.standard.removeObject(forKey: "\(id)-show")
            }
        }
    }
    
    var source: Source? = nil {
        didSet {
            if let source = source, let data = try? JSONEncoder().encode(source) {
                UserDefaults.standard.set(data, forKey: "\(id)-source")
            } else {
                UserDefaults.standard.removeObject(forKey: "\(id)-source")
            }
        }
    }
    
    init(id: String) {
        self.id = id
        
        if let data = UserDefaults.standard.data(forKey: "\(id)-year"),
            let year = try? JSONDecoder().decode(Year.self, from: data) {
            self.year = year
        }
        
        if let data = UserDefaults.standard.data(forKey: "\(id)-show"),
            let show = try? JSONDecoder().decode(Show.self, from: data) {
            self.show = show
        }
        
        if let data = UserDefaults.standard.data(forKey: "\(id)-source"),
            let source = try? JSONDecoder().decode(Source.self, from: data) {
            self.source = source
        }
    }
}
