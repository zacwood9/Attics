//
//  Song.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

struct Song: Equatable {
    let id: Int
    let title: String
    let fileName: String
    
    let source: Source
    
    var downloadURL: URL {
        let urlString = "http://archive.org/download/\(self.source.identifier)/\(self.fileName)".addingPercentEncoding(withAllowedCharacters: .urlFragmentAllowed)!
        return URL(string: urlString)!
    }
}
