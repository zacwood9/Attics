//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/12/23.
//

import Foundation

class LegacyFileStore {
    static func getStoredRecordings() -> [Legacy.StoredRecording] {
        if let file = try? Folder.applicationSupport.file(named: "storage.json"),
           let data = try? file.read(),
           let value = try? JSONDecoder().decode([Legacy.StoredRecording].self, from: data) {
            return value
        }
        
        return []
    }
}
