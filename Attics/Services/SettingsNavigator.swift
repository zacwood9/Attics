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
        
        navigationController.navigationBar.prefersLargeTitles = false
        navigationController.navigationBar.barTintColor = #colorLiteral(red: 0.1997258663, green: 0.2665995359, blue: 0.5491077304, alpha: 1)
        navigationController.navigationBar.isTranslucent = false
        navigationController.navigationBar.largeTitleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.titleTextAttributes = [.foregroundColor : UIColor.white]
        navigationController.navigationBar.tintColor = .white
    }
    
    func openSafari(in url: String) {
        navigationController.viewControllers.last?.presentSafariViewController(at: URL(string: url)!)
    }
}
