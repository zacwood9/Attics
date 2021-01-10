//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Network
import CoreData

final class MyShowsNavigator: AtticsNavigator {
    var state = AppState(id: "myShows")
        
    init(storage: AppStorageManager, networkStatus: @escaping () -> NWPath.Status) {
        let storyboard = UIStoryboard(name: "Main", bundle: nil)
        let sourcesVC = storyboard.instantiateViewController(withIdentifier: ViewControllerIds.myShows) as! MyShowsViewController
        sourcesVC.storage = storage
        
        super.init(rootViewController: sourcesVC, storage: storage, networkStatus: networkStatus)
        sourcesVC.onChangeBandTap = presentBandsController
        sourcesVC.onSourceTap = pushRecordingController

        let image = UIImage(systemName: "heart.fill")
        navigationController.tabBarItem = UITabBarItem(title: "My Shows", image: image, tag: 1)
    }
    
    // UINavigationControllerDelegate method to update persistant state when screens change
    func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated: Bool) {
        
    }
}
