//
//  LegacyModels.swift
//  Attics
//
//  Created by Zachary Wood on 12/26/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import Foundation

struct LegacyBand: Codable {
    let collection: String
    let name: String
}

struct LegacyYear: Codable, Equatable, Hashable {
    let collection: String
    let year: String
}

struct LegacyShow: Codable, Equatable, Hashable {
    let collection: String
    let date: String
    let venue: String
    let city: String
    let state: String
    let numReviews: Int
    let numSources: Int
    let avgRating: Double
    let year: LegacyYear
}


struct LegacySource: Codable, Equatable, Hashable {
    let identifier: String
    let collection: String
    let transferer: String
    let source: String
    let avgRating: Double
    let downloads: Int
    let numReviews: Int
    let lineage: String
    let isFavorite: Bool
    let show: LegacyShow
}


struct LegacySong: Equatable, Hashable, Codable {
    let title: String
    let fileName: String
    let album: String?
    let track: Int
    let length: String
    let source: LegacySource
}
