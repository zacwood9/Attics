//
//  SettingsNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import StoreKit
import SwiftUI

final class SettingsNavigator {
    let navigationController: UINavigationController
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    let storage: AppStorageManager
    
    init(storage: AppStorageManager) {
        self.storage = storage
        
        navigationController = UINavigationController()
        navigationController.tabBarItem = UITabBarItem(tabBarSystemItem: .more, tag: 1)
        navigationController.navigationBar.isHidden = true
//        navigationController.navigationBar.prefersLargeTitles = false
//        navigationController.navigationBar.barTintColor = #colorLiteral(red: 0.1997258663, green: 0.2665995359, blue: 0.5491077304, alpha: 1)
//        navigationController.navigationBar.isTranslucent = false
//        navigationController.navigationBar.largeTitleTextAttributes = [.foregroundColor : UIColor.white]
//        navigationController.navigationBar.titleTextAttributes = [.foregroundColor : UIColor.white]
//        navigationController.navigationBar.tintColor = .white
        
        let settingsVC = UIHostingController(rootView: SettingsView(openSafari: { a in self.openSafari(in:a) }, openReview: { self.openReview() } , removeAllDownloads: { try? self.storage.removeAllDownloads() }))
        navigationController.pushViewController(settingsVC, animated: false)
    }
    
    private func openSafari(in url: String) {
        navigationController.viewControllers.last?.openSafari(to: URL(string: url)!)
    }
    
    private func openReview() {
        SKStoreReviewController.requestReview()
    }
}
