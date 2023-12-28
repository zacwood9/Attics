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
        case legacyStorage = "storage.json"
    }
    
    let applicationSupport: Folder
    let downloads: Folder
    let db: Connection
    
    public let bandRepository: BandRepository
    public let performanceRepository: PerformanceRepository
    public let recordingRepository: RecordingRepository
    public let trackRepository: TrackRepository
    
    public init() throws {
        let path = "Library/Application Support"
        self.applicationSupport = try Folder.home.createSubfolderIfNeeded(at: path)
        self.downloads = try applicationSupport.createSubfolderIfNeeded(withName: "Downloads")
        
        let sqliteFile = try applicationSupport.createFileIfNeeded(withName: "database.sqlite3")
        self.db = try Connection(sqliteFile.path)
        
        self.bandRepository = BandRepository(db: db)
        self.performanceRepository = PerformanceRepository(db: db)
        self.recordingRepository = RecordingRepository(db: db)
        self.trackRepository = TrackRepository(db: db)
        
        try bandRepository.loadSchema()
        try performanceRepository.loadSchema()
        try recordingRepository.loadSchema()
        try trackRepository.loadSchema()
    }
    
    public func registerDownload(sourceUrl: URL, identifier: String, fileName: String) throws {
        let file = try File.init(path: sourceUrl.absoluteString)
        let identifierFolder = try downloads.createSubfolderIfNeeded(withName: identifier)
        try file.move(to: identifierFolder)
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
    
    public func trackUrl(recordingIdentifier: String, fileName: String) -> URL? {
        do {
            let recordingFolder = try downloads.subfolder(named: recordingIdentifier)
            let trackFile = try recordingFolder.file(named: fileName)
            return trackFile.url
        } catch {
            return nil
        }
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

