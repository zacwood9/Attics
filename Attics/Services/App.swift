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

final class App {
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    let browseNavigator = BrowseNavigator()
    let downloadsController = UINavigationController()
    let settingsNavigator = SettingsNavigator()
    
    let tabBarController: UITabBarController
    let dataStore = NetworkDataStore()
    
    init(window: UIWindow) {
        tabBarController = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.mainTabBar) as! UITabBarController
        tabBarController.setViewControllers([browseNavigator.navigationController, downloadsController, settingsNavigator.navigationController], animated: true)
        window.rootViewController = tabBarController
        
        let downloadsVC = storyboard.instantiateViewController(withIdentifier: "DownloadsViewController")
        downloadsController.pushViewController(downloadsVC, animated: false)
        downloadsController.tabBarItem = UITabBarItem(tabBarSystemItem: .downloads, tag: 1)
        configureAudio()
    }
    
    private func configureAudio() {
        try! AVAudioSession.sharedInstance().setActive(true, options: AVAudioSession.SetActiveOptions.notifyOthersOnDeactivation)
        try! AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        
        UIApplication.shared.beginReceivingRemoteControlEvents()
    }
}
