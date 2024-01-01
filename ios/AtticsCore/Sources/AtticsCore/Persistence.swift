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
        case selectedTab = "v2SelectedTab.json"
        case legacyStorage = "storage.json"
    }
    
    let applicationSupport: Folder
    let downloads: Folder
    public let images: Folder
    let db: Connection
    
    public let bandRepository: BandRepository
    public let performanceRepository: PerformanceRepository
    public let recordingRepository: RecordingRepository
    public let trackRepository: TrackRepository
    
    public init() throws {
        applicationSupport = try Folder.home.createSubfolderIfNeeded(at: "Library/Application Support")
        
        downloads = try applicationSupport.createSubfolderIfNeeded(withName: "Downloads")
        downloads.excludeForBackup()
        
        images = try applicationSupport.createSubfolderIfNeeded(withName: "Images")
        images.excludeForBackup()
        
        let sqliteFile = try applicationSupport.createFileIfNeeded(withName: "database.sqlite3")
        sqliteFile.excludeFromBackup()
        
        db = try Connection(sqliteFile.path)
        
        bandRepository = BandRepository(db: db)
        performanceRepository = PerformanceRepository(db: db)
        recordingRepository = RecordingRepository(db: db)
        trackRepository = TrackRepository(db: db)
        
        try migrate()
    }
    
    public func registerDownload(sourceUrl: URL, identifier: String, fileName: String) throws {
        let file = try File.init(path: sourceUrl.path())
        let identifierFolder = try downloads.createSubfolderIfNeeded(withName: identifier)
        try file.move(to: identifierFolder)
        try file.rename(to: fileName, keepExtension: false)
        
        logger.info("Moved downloaded file \(fileName) to \(identifier)/\(fileName) for permanant storage.")
    }
    
    public func loadDecodable<T: Decodable>(at path: Path) throws -> T {
        let file = try applicationSupport.file(named: path.rawValue)
        let data = try file.read()
        
        return try JSONDecoder().decode(T.self, from: data)
    }
    
    public func persistEncodable<T: Encodable>(_ value: T, to path: Path) throws {
        let file = try applicationSupport.createFileIfNeeded(withName: path.rawValue)
        file.excludeFromBackup()
        
        let data = try JSONEncoder().encode(value)
        try file.write(data)
    }
    
    public func trackUrl(recordingIdentifier: String, fileName: String) -> URL {
        do {
            let recordingFolder = try downloads.subfolder(named: recordingIdentifier)
            let trackFile = try recordingFolder.file(named: fileName)
            return trackFile.url
        } catch {            
            return URL(
                string: "https://archive.org/download/\(recordingIdentifier)/\(fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
            )!
        }
    }
    
    private func migrate() throws {        
        if db.userVersion == 0 {
            try db.run(BandRepository.table.create(ifNotExists: true) { t in
                typealias Columns = BandRepository.Rows
                t.column(Columns.id, primaryKey: true)
                t.column(Columns.name, unique: true)
                t.column(Columns.collection, unique: true)
            })
            
            try db.run(PerformanceRepository.table.create(ifNotExists: true) { t in
                typealias Columns = PerformanceRepository.Rows
                t.column(Columns.id, primaryKey: true)
                t.column(Columns.date)
                t.column(Columns.venue)
                t.column(Columns.city)
                t.column(Columns.state)
                t.column(Columns.bandId)
                
                t.foreignKey(Columns.bandId, references: BandRepository.table, BandRepository.Rows.id)
            })
            
            try db.run(RecordingRepository.table.create(ifNotExists: true) { t in
                typealias Rows = RecordingRepository.Rows
                t.column(Rows.id, primaryKey: true)
                t.column(Rows.identifier)
                t.column(Rows.transferer)
                t.column(Rows.source)
                t.column(Rows.lineage)
                t.column(Rows.favorite, defaultValue: false)
                t.column(Rows.downloaded, defaultValue: false)
                t.column(Rows.performanceId)
                
                t.foreignKey(Rows.performanceId, references: PerformanceRepository.table, PerformanceRepository.Rows.id)
            })
            
            try db.run(TrackRepository.table.create(ifNotExists: true) { t in
                typealias Rows = TrackRepository.Rows
                t.column(Rows.id, primaryKey: true)
                t.column(Rows.title)
                t.column(Rows.fileName)
                t.column(Rows.track)
                t.column(Rows.length)
                t.column(Rows.recordingId)
                
                t.foreignKey(Rows.recordingId, references: RecordingRepository.table, RecordingRepository.Rows.id)
            })
            
            try db.run(PendingImport.table.create() { t in
                typealias Columns = PendingImport.Columns
                t.column(Columns.id, primaryKey: true)
                t.column(Columns.recordingId)
                
                t.foreignKey(Columns.recordingId, references: RecordingRepository.table, RecordingRepository.Rows.id)
            })
            
            db.userVersion = 1
        }
        if db.userVersion == 1 {
            try db.run(PendingImport.table.addColumn(PendingImport.Columns.pending, defaultValue: false))
            
            db.userVersion = 2
        }
        if db.userVersion == 2 {
            try db.run(TrackPlay.table.create() { t in
                typealias C = TrackPlay.Columns
                t.column(C.id, primaryKey: true)
                t.column(C.trackId)
                t.column(C.playedAt)
                                
                t.foreignKey(C.trackId, references: TrackRepository.table, TrackRepository.Rows.id)
            })
            
            db.userVersion = 3
        }
        
        logger.info("Database migrated.")
    }
}

extension File {
    func excludeFromBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        do {
            try urlCopy.setResourceValues(vals)
        } catch {
            logger.error("Failed to exclude file \(url) from backup: \(error)")
        }
    }
}

extension Folder {
    func excludeForBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        do {
            try urlCopy.setResourceValues(vals)
        } catch {
            logger.error("Failed to exclude folder \(url) from backup: \(error)")
        }
    }
}

