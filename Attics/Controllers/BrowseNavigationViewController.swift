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
        let yearsViewController = YearsViewController(dataStore: dataStore)

        self.init(rootViewController: yearsViewController)
        
        navigationBar.prefersLargeTitles = true
        navigationItem.largeTitleDisplayMode = .automatic
    }
    
}
