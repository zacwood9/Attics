//
//  FileCache.swift
//  Attics
//
//  Created by Zachary Wood on 1/2/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import Foundation

class FileCache<T: Codable> {
    let file: File
    
    init(named name: String) {
        let cacheDir = try! Folder.home.subfolder(at: "Library/Caches")
        file = try! cacheDir.createFileIfNeeded(withName: name)
        if try! file.readAsString().isEmpty {
            try! file.write("{}")
        }
    }
    
    func get() throws -> T  {
        return try JSONDecoder().decode(T.self, from: file.read())
    }
    
    func set(to item: T) throws {
        try file.write(try JSONEncoder().encode(item))
    }
}
