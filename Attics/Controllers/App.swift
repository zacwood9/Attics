//
//  App.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import AVKit

final class App {
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    let browseController: UINavigationController
    let tabBarController: UITabBarController
    let dataStore: NetworkDataStore
    
    private func configureAudio() {
        try! AVAudioSession.sharedInstance().setActive(true, options: AVAudioSession.SetActiveOptions.notifyOthersOnDeactivation)
        try! AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        
//        try! AVAudioSession.sharedInstance().setCategory(AVAudioSession.Category(rawValue: convertFromAVAudioSessionCategory(AVAudioSession.Category.playback)), mode: AVAudioSession.Mode.default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
    }
    
    func pushShowsController(year: Year) {
        let showsVC = storyboard.instantiateViewController(withIdentifier: "ShowsController") as! ShowsViewController
        showsVC.year = year
        showsVC.dataStore = dataStore
        browseController.pushViewController(showsVC, animated: true)
    }
    
    func pushSourcesController(show: Show) {
        
    }
    
    init(window: UIWindow) {
        let yearsViewController = storyboard.instantiateViewController(withIdentifier: "YearsTopShows") as! YearsViewController
        browseController = UINavigationController(rootViewController: yearsViewController)
        
        tabBarController = storyboard.instantiateViewController(withIdentifier: "MainTabBarController") as! UITabBarController
        tabBarController.setViewControllers([browseController], animated: true)
        window.rootViewController = tabBarController
        
        dataStore = NetworkDataStore()
        
        yearsViewController.dataStore = dataStore
        yearsViewController.onYearTapped = pushShowsController
        browseController.navigationBar.prefersLargeTitles = true
        browseController.tabBarItem = UITabBarItem(tabBarSystemItem: .search, tag: 1)

        configureAudio()
    }
}
