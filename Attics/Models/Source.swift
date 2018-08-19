//
//  Source.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Source: Equatable {
    let id: Int
    let identifier: String
    let transferer: String
    let source: String
    let avgRating: Double
    let downloads: Int
    let numReviews: Int
    let description: String
    let lineage: String
    
    let show: Show
    
//    var metadataUrl: URL {
//        return URL(string: "https://archive.org/metadata/\(identifier)")!
//    }
}
