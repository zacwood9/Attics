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
    
    public func loadSchema() throws {
        try db.run(table.create(ifNotExists: true) { t in
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
    }
    
    public func get(id: String) throws -> Recording? {
        return try db.pluck(table.filter(Rows.id == id)).map { try $0.decode() }
    }
    
    public func insertApi(_ api: APIRecording) throws -> Recording {
        let recording = Recording.fromApi(apiRecording: api)
        try db.run(table.insert(recording))
        return recording
    }
    
    public func getFavorites() throws -> [Recording] {
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
}
