//
//  Source.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Source: Equatable, Codable {
    let id: Int
    let transferer: String?
    let identifier: String
    let showId: Int
    
    var metadataUrl: URL {
        return URL(string: "https://archive.org/metadata/\(identifier)")!
    }
}
