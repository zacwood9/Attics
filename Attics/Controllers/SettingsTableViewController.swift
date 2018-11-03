//
//  SettingsTableViewController.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class SettingsTableViewController: UITableViewController {
    
    var openSafari: (String) -> () = { _ in }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch (indexPath.section, indexPath.row) {
        case (0, 0): openSafari("https://archive.org/about/faqs.php#1177")
        case (1, 0): openSafari("https://twitter.com/_zacwood")
        case (1, 1): openSafari("https://github.com/zacwood9/attics")
        default: fatalError("row not implemented")
        }
    }
}
