//
//  Refreshable.swift
//  Attics
//
//  Created by Zachary Wood on 8/27/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

@objc protocol Refreshable {
    var refreshControl: UIRefreshControl { get set }
    func refresh()
}

