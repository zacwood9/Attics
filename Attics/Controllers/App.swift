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
    
    private func configureAudio() {
        try! AVAudioSession.sharedInstance().setActive(true, options: AVAudioSession.SetActiveOptions.notifyOthersOnDeactivation)
        try! AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        
//        try! AVAudioSession.sharedInstance().setCategory(AVAudioSession.Category(rawValue: convertFromAVAudioSessionCategory(AVAudioSession.Category.playback)), mode: AVAudioSession.Mode.default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
    }
    
    func pushYearController() {
        
    }
    
    init(window: UIWindow) {
        browseController = BrowseNavigationViewController()
        browseController.tabBarItem = UITabBarItem(tabBarSystemItem: .search, tag: 1)
        
        tabBarController = storyboard.instantiateViewController(withIdentifier: "MainTabBarController") as! UITabBarController
        tabBarController.setViewControllers([browseController], animated: true)
        window.rootViewController = tabBarController

        configureAudio()
        pushYearController()
    }
}
