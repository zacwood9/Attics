//
//  YearsTableViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class YearsTableViewCell: UITableViewCell {
    @IBOutlet weak var yearLabel: UILabel!
    @IBOutlet weak var topShowsView: UICollectionView!
    @IBOutlet weak var viewDetailLabel: UILabel!
    
    var collectionViewOffset: CGFloat {
        get {
            return topShowsView.contentOffset.x
        }
        
        set {
            topShowsView.contentOffset.x = newValue
        }
    }
    
    func setCollectionViewDataSourceDelegate
        <D: UICollectionViewDataSource & UICollectionViewDelegate>
        (dataSourceDelegate: D, forRow row: Int) {
        
        topShowsView.delegate = dataSourceDelegate
        topShowsView.dataSource = dataSourceDelegate
        topShowsView.tag = row
        topShowsView.reloadData()
    }
}
