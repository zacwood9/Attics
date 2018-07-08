//
//  Song.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Song: Equatable, Codable {
    let title: String
    let fileName: String
    let album: String
    
    enum CodingKeys: String, CodingKey {
        case title, album
        case fileName = "name"
    }
    
//    var downloadURL: URL {
//        let urlString = "http://archive.org/download/\(self.source.identifier)/\(self.fileName)".addingPercentEncoding(withAllowedCharacters: .urlFragmentAllowed)!
//        return URL(string: urlString)!
//    }
}
