//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/12/23.
//

import Foundation

struct APIResources {
    struct BandWithMetadata: Codable {
        let id: String
        let collection: String
        let name: String
        let logoUrl: String
        let numPerformances: Int
        let numRecordings: Int
    }
}
