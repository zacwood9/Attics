//
//  SettingsNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

final class SettingsNavigator {
    let navigationController: UINavigationController
    let storyboard = UIStoryboard(name: "Main", bundle: nil)
    
    init() {
        let settingsVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.settings) as! SettingsTableViewController
        navigationController = UINavigationController(rootViewController: settingsVC)
        navigationController.tabBarItem = UITabBarItem(tabBarSystemItem: .more, tag: 1)
        settingsVC.navigationItem.title = "More"
        settingsVC.openSafari = openSafari
    }
    
    func openSafari(in url: String) {
        navigationController.viewControllers.last?.presentSafariViewController(at: URL(string: url)!)
    }
}
