//
//  Show.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

struct Show: Codable, Equatable, Hashable {
    let date: String
    let venue: String
    let city: String
    let state: String
    let numReviews: Int
    let numRecordings: Int
    let avgRating: Double
    let bandId: String
}
