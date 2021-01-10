//
//  App.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import AVKit
import CoreData
import Network

// App is the top level view coordinator. It holds the main TabBarController which
// is the highest level view and contains the different navigators for all the tabs.
final class App: NSObject {
    static var shared = App()
    
    var storyboard: UIStoryboard!
    var tabBarController: ATSTabBarController!
    
    var browseNavigator: BrowseNavigator!
    var myShowsNavigator: MyShowsNavigator!
    lazy var settingsNavigator = SettingsNavigator(storage: storage)
    let api = APIClient()
    
    let favoritesStore = FileSystemSourceStore(named: "favorites.json")
    let downloadsStore = FileSystemSourceStore(named: "downloads.json")
    
    let monitor = NWPathMonitor()
    
    var downloadManagers: [Source : DownloadManager_] = [:]
    
    /// Persistantly store what tab is open
    var tabState = UserDefaultStore<Int>(withKey: "tabIndex", defaultValue: 0)
    
    /// Is the app being launched for the first time? Key changes with each update
    var firstLaunch = UserDefaultStore<Bool>(withKey: "v1.4update", defaultValue: true)
    
    lazy var musicPlayer = MusicPlayer(storage: storage)
    
    private override init() {
        super.init()
        monitor.start(queue: .main)
    }
    
    func attachToWindow(_ window: UIWindow) {
        configureNavigators()
        configureGlobalNavigation()
        
        window.rootViewController = tabBarController
    }
    
    func networkStatus() -> NWPath.Status {
        monitor.currentPath.status
    }
    
    let storage = AppStorageManager()
    
    // MARK: - Configuration
    
    private func configureNavigators() {
        browseNavigator = BrowseNavigator(
            storage: storage,
            apiClient: api,
            networkStatus: networkStatus
        )
        
        myShowsNavigator = MyShowsNavigator(
            storage: storage,
            networkStatus: networkStatus
        )
        
        self.tabBarController = ATSTabBarController()
        self.tabBarController.shouldPresentUpdate = { [unowned self] in
//            self.firstLaunch.item!
            true
        }
        self.tabBarController.didPresentUpdate = { [unowned self] in
            self.firstLaunch.item = false
        }
        self.tabBarController.setViewControllers(
            [browseNavigator.navigationController, myShowsNavigator.navigationController, settingsNavigator.navigationController],
            animated: true
        )
        
        self.tabBarController.selectedIndex = tabState.item ?? 0
        self.tabBarController.delegate = self
    }
    
    private func configureGlobalNavigation() {
        let coloredAppearance = UINavigationBarAppearance()
        coloredAppearance.configureWithOpaqueBackground()
        coloredAppearance.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
        coloredAppearance.titleTextAttributes = [.foregroundColor: UIColor.white]
        coloredAppearance.largeTitleTextAttributes = [.foregroundColor: UIColor.white]
        
        UINavigationBar.appearance().standardAppearance = coloredAppearance
        UINavigationBar.appearance().scrollEdgeAppearance = coloredAppearance
    }
    
    func openUrl(_ path: String, _ params: [URLQueryItem]) {
        tabBarController.selectedIndex = 0
        
        switch path {
        case "/ShowRecording":
            guard let identifier = params.first(where: { $0.name == "identifier" })?.value else { return }
            browseNavigator.openToRecording(identifier: identifier)
            
        default: return
        }
    }
    
    
}

struct NavigationState {
    var tab: Int
    var band: Band
    var year: Year?
}

// MARK: - Extensions


extension App: UITabBarControllerDelegate {
    // Save the tab state when the tab is changed
    func tabBarController(_ tabBarController: UITabBarController, didSelect viewController: UIViewController) {
        tabState.item = tabBarController.selectedIndex
    }
}

public extension Notification.Name {
    static let BandDidChange = Notification.Name("BandDidChange")
}
