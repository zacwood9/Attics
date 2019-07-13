//
//  SourceCollectionViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Cosmos

class SourceCollectionViewCell: UICollectionViewCell {
    @IBOutlet weak var lineageLabel: UILabel!
    @IBOutlet weak var downloadsLabel: UILabel!
    @IBOutlet weak var reviewsLabel: UILabel!
    @IBOutlet weak var transfererLabel: UILabel!
    @IBOutlet weak var recordingTypeLabel: UILabel!
    @IBOutlet weak var stars: CosmosView!
    @IBOutlet weak var view: UIView!
    @IBOutlet weak var recordingTypeWrapper: UIView!
}
