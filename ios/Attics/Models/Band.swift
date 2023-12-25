//
//  Band.swift
//  Attics
//
//  Created by Zachary Wood on 8/19/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

struct Band: Codable {
    let collection: String
    let name: String
    let logoUrl: String
}

struct BandWithMetadata: Codable {
    let id: String
    let collection: String
    let name: String
    let logoUrl: String
    let numPerformances: Int
    let numRecordings: Int
}
