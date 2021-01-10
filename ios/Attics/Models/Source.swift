//
//  Source.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import CoreData

struct Source: Codable, Equatable, Hashable {
    let id: String
    let identifier: String
    let transferer: String
    let source: String
    let avgRating: Double
    let atticsDownloads: Int
    let archiveDownloads: Int
    let numReviews: Int
    let lineage: String
    let performanceId: String
    
    var downloads: Int {
        atticsDownloads + archiveDownloads
    }
}

extension Source {
    enum RecordingType: String {
        case sbd = "SBD", aud = "AUD", matrix = "MATRIX", unknown = ""
    }
    
    var type: RecordingType {
        let s = source.lowercased()
        for type in ["mtx", "matrix"] {
            if s.contains(type) { return .matrix }
        }
        for type in ["aud"] {
            if s.contains(type) { return .aud }
        }
        for type in ["sbd", "soundboard"] {
            if s.contains(type) { return .sbd }
        }
        return .unknown
    }
}
