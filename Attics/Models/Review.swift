//
//  Review.swift
//  Attics
//
//  Created by Zachary Wood on 7/23/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Review: Equatable {
    let author: String
    let title: String
    let body: String
    let date: Date
    let stars: Int
}
