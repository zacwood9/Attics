//
//  File.swift
//
//
//  Created by Zachary Wood on 12/23/23.
//

import Foundation
import SQLite

public struct Recording: Codable {
    public let id: String
    public let identifier: String
    public let transferer: String
    public let source: String
    public let lineage: String
    public let favorite: Bool
    public let downloaded: Bool
    public let performanceId: String
}

extension Recording {
    public static func fromApi(apiRecording: APIRecording, favorite: Bool = false, downloaded: Bool = false) -> Self {
        self.init(id: apiRecording.id, identifier: apiRecording.identifier, transferer: apiRecording.transferer, source: apiRecording.source, lineage: apiRecording.lineage, favorite: favorite, downloaded: downloaded, performanceId: apiRecording.performanceId)
    }
}

public class RecordingRepository {
    let db: Connection
    
    static let table = Table("recordings")
    var table: Table { RecordingRepository.table }
    
    struct Rows {
        static let id = Expression<String>("id")
        static let identifier = Expression<String>("identifier")
        static let transferer = Expression<String>("transferer")
        static let source = Expression<String>("source")
        static let lineage = Expression<String>("lineage")
        static let favorite = Expression<Bool>("favorite")
        static let downloaded = Expression<Bool>("downloaded")
        static let performanceId = Expression<String>("performanceId")
    }
    
    public init(db: Connection) {
        self.db = db
    }
    
    public func get(id: String) throws -> Recording? {
        logger.debug("RecordingRepository.get(id: \(id))")
        
        return try db.pluck(table.filter(Rows.id == id)).map { try $0.decode() }
    }
    
    public func insertApi(_ api: APIRecording) throws -> Recording {
        let recording = Recording.fromApi(apiRecording: api)
        try db.run(table.insert(recording))
        return recording
    }
    
    @discardableResult
    public func upsert(id: String, identifier: String, transferer: String, source: String, lineage: String, favorite: Bool, downloaded: Bool, performanceId: String) throws -> Recording {
        let recording = Recording(id: id, identifier: identifier, transferer: transferer, source: source, lineage: lineage, favorite: favorite, downloaded: downloaded, performanceId: performanceId)
        try db.run(table.upsert(recording, onConflictOf: Rows.id))
        return recording
    }
    
    public func getFavorites() throws -> [Recording] {
        logger.debug("RecordingRepository.getFavorites()")
        
        let rows = try db.prepare(table.where(Rows.favorite))
        return rows.compactMap {
            do {
                let recording: Recording = try $0.decode()
                return recording
            } catch {
                print(error)
                return nil
            }
        }
    }
    
    public func getDownloads() throws -> [Recording] {
        logger.debug("RecordingRepository.getDownloads()")
        
        let rows = try db.prepare(table.where(Rows.downloaded))
        return rows.compactMap { decode(row: $0) }
    }
    
    public func getPersisted() throws -> [Recording] {
        logger.debug("RecordingRepository.getPersisted()")
        
        let rows = try db.prepare(table.where(Rows.downloaded || Rows.favorite))
        return rows.compactMap { decode(row: $0) }
    }
    
    public func all() throws -> [Recording] {
        logger.debug("RecordingRepository.all()")
        
        let rows = try db.prepare(table)
        return rows.compactMap { decode(row: $0) }
    }
    
    private func decode(row: Row) -> Recording? {
        do {
            let recording: Recording = try row.decode()
            return recording
        } catch {
            logger.error("Failed to decode Recording: \(error)")
            return nil
        }
    }
    
    @discardableResult
    public func favorite(id: String) throws -> Recording? {
        try db.run(table.filter(Rows.id == id).update(Rows.favorite <- true))
        return try get(id: id)
    }
    
    @discardableResult
    public func unfavorite(id: String) throws -> Recording? {
        try db.run(table.filter(Rows.id == id).update(Rows.favorite <- false))
        return try get(id: id)
    }
    
    @discardableResult
    public func markDownloaded(id: String) throws -> Recording? {
        try db.run(table.filter(Rows.id == id).update(Rows.downloaded <- true))
        return try get(id: id)
    }
    
    @discardableResult
    public func markUndownloaded(id: String) throws -> Recording? {
        try db.run(table.filter(Rows.id == id).update(Rows.downloaded <- false))
        return try get(id: id)
    }
    
    public func removeAllDownloads() throws {
        try db.run(table.update(Rows.downloaded <- false))
    }
}
