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
        
        let settingsVC = UIHostingController(
            rootView:
                SettingsView(
                    openSafari: { a in self.openSafari(in:a) },
                    openReview: { self.openReview() } ,
                    removeAllDownloads: { try? self.storage.removeAllDownloads() },
                    openUpdate: openUpdate
                )
        )
        navigationController.pushViewController(settingsVC, animated: false)
    }
    
    private func openSafari(in url: String) {
        navigationController.viewControllers.last?.openSafari(to: URL(string: url)!)
    }
    
    private func openReview() {
        SKStoreReviewController.requestReview()
    }
    
    func openUpdate() {
        navigationController.present(UpdateVC(), animated: true, completion: nil)
    }
}
