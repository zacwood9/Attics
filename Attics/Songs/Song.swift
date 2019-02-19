//
//  Song.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Song: Equatable {
    let title: String
    let fileName: String
    let album: String?
    let track: Int
    let length: String
    let source: Source
}
