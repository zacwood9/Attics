//
//  App.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import AVKit
import FontAwesome
import MediaPlayer
import CoreData

final class App: NSObject, UITabBarControllerDelegate {
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    let browseNavigator = BrowseNavigator()
    let myShowsNavigator = MyShowsNavigator()
    let downloadsController = UINavigationController()
    let settingsNavigator = SettingsNavigator()
    
    let tabBarController: UITabBarController
    let dataStore = NetworkDataStore()
    
    var tabState = TabState()
    
    init(window: UIWindow) {
        tabBarController = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.mainTabBar) as! UITabBarController
        super.init()
        
        tabBarController.setViewControllers([
            browseNavigator.navigationController,
            myShowsNavigator.navigationController,
            settingsNavigator.navigationController],  animated: true)        
        window.rootViewController = tabBarController
        tabBarController.selectedIndex = tabState.tab
        tabBarController.delegate = self
        configureAudio()
    }
    
    func tabBarController(_ tabBarController: UITabBarController, didSelect viewController: UIViewController) {
        tabState.tab = tabBarController.selectedIndex
    }
    
    private func configureAudio() {
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
    }
}

struct TabState {
    var tab: Int {
        get {
            return UserDefaults.standard.integer(forKey: "tabIndex")
        }
        set {
            print(newValue)
            UserDefaults.standard.set(newValue, forKey: "tabIndex")
        }
    }
}
