//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/23/23.
//

import Foundation
import Files
import SQLite

public class Persistence {
    public enum PersistenceError: Error {
        case fileNotFound(String)
        case readError
        case encodingError
        case writeError
    }
    
    public enum Path: String {
        case playlist = "v2playlist.json"
        case navigation = "v2navigation.json"
        case libraryNavigation = "v2library.json"
    }
    
    let applicationSupport: Folder
    let sqliteConnection: Connection
    
    public let bandRepository: BandRepository
    public let performanceRepository: PerformanceRepository
    public let recordingRepository: RecordingRepository
    public let trackRepository: TrackRepository
    
    public init() throws {
        let path = "Library/Application Support"
        if Folder.home.containsSubfolder(at: path) {
            self.applicationSupport = try Folder.home.subfolder(at: path)
        } else {
            self.applicationSupport = try Folder.home.createSubfolder(at: path)
        }
        
        let sqliteFile = try applicationSupport.createFileIfNeeded(withName: "database.sqlite3")
        self.sqliteConnection = try Connection(sqliteFile.path)
        
        self.bandRepository = BandRepository(db: sqliteConnection)
        self.performanceRepository = PerformanceRepository(db: sqliteConnection)
        self.recordingRepository = RecordingRepository(db: sqliteConnection)
        self.trackRepository = TrackRepository(db: sqliteConnection)
        
        try bandRepository.loadSchema()
        try performanceRepository.loadSchema()
        try recordingRepository.loadSchema()
        try trackRepository.loadSchema()
    }
    
    public func loadDecodable<T: Decodable>(at path: Path) throws -> T {
        let file = try applicationSupport.file(named: path.rawValue)
        let data = try file.read()
        
        return try JSONDecoder().decode(T.self, from: data)
    }
    
    public func persistEncodable<T: Encodable>(_ value: T, to path: Path) throws {
        let file = try applicationSupport.createFileIfNeeded(withName: path.rawValue)
        let data = try JSONEncoder().encode(value)
        try file.write(data)
    }
}

extension Folder {
    static var applicationSupport: Folder {
        let path = "Library/Application Support"
        guard Folder.home.containsSubfolder(at: path) else {
            return try! Folder.home.createSubfolder(at: path)
        }
        return try! Folder.home.subfolder(at: path)
    }
}

extension File {
    func excludeFromBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        try! urlCopy.setResourceValues(vals)
    }
}

extension Folder {
    func excludeForBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        try! urlCopy.setResourceValues(vals)
    }
}

