//
//  BrowseNavigationViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import LNPopupController

class BrowseNavigationViewController: UINavigationController {

    override func viewDidLoad() {
        super.viewDidLoad()
    }
    
    convenience init() {
        let dataStore = NetworkDataStore()
        let storyboard = UIStoryboard.init(name: "Main", bundle: nil)
        
        let yearsViewController = storyboard.instantiateViewController(withIdentifier: "YearsTopShows") as! YearsViewController
        
        yearsViewController.dataStore = NetworkDataStore()

        self.init(rootViewController: yearsViewController)
        
        navigationBar.prefersLargeTitles = true
        navigationItem.largeTitleDisplayMode = .automatic
    }
    
}
