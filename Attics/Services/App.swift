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

final class App {
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    let browseNavigator = BrowseNavigator()
    let myShowsNavigator = MyShowsNavigator()
    let downloadsController = UINavigationController()
    let settingsNavigator = SettingsNavigator()
    
    let tabBarController: UITabBarController
    let dataStore = NetworkDataStore()
    
    init(window: UIWindow) {
        tabBarController = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.mainTabBar) as! UITabBarController
        tabBarController.setViewControllers([
            browseNavigator.navigationController,
            myShowsNavigator.navigationController,
            settingsNavigator.navigationController],  animated: true)
        window.rootViewController = tabBarController
        
        configureAudio()
        
        func getSizeOfUserDefaults() -> Int? {
            guard let libraryDir = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.libraryDirectory, FileManager.SearchPathDomainMask.userDomainMask, true).first else {
                return nil
            }
            
            guard let bundleIdentifier = Bundle.main.bundleIdentifier else {
                return nil
            }
            
            let filepath = "\(libraryDir)/Preferences/\(bundleIdentifier).plist"
            let filesize = try? FileManager.default.attributesOfItem(atPath: filepath)
            let retVal = filesize?[FileAttributeKey.size]
            return retVal as? Int
        }
        print(getSizeOfUserDefaults() ?? 0)
    }
    
    private func configureAudio() {
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        UIApplication.shared.beginReceivingRemoteControlEvents()
    }
}
