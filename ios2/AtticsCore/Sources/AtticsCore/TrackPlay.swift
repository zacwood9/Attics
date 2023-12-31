//
//  History.swift
//
//
//  Created by Zachary Wood on 12/29/23.
//

import Foundation
import SQLite

struct History : Codable {
    static let table = Table("track_plays")
    enum Columns {
        static let id = Expression<String>("id")
        static let pending = Expression<Bool>("pending")
        static let recordingId = Expression<String>("recordingId")
    }
    
    let id: String
    let pending: Bool
    let recordingId: String
    
    var table: Table { PendingImport.table }
    
    static func all(_ p: Persistence) throws -> [PendingImport] {
        let rows = try p.db.prepare(table)
        return rows.compactMap { Self.decode(row: $0) }
    }
    
    static func allPending(_ p: Persistence) throws -> [PendingImport] {
        let rows = try p.db.prepare(table.where(Columns.pending))
        return rows.compactMap { Self.decode(row: $0) }
    }
    
    @discardableResult
    static func create(_ p: Persistence, recordingId: String) throws -> PendingImport {
        let pendingImport = PendingImport(id: UUID().uuidString, pending: true, recordingId: recordingId)
        try p.db.run(table.insert(pendingImport))
        logger.info("Created pending import for recording \(recordingId)")
        return pendingImport
    }
    
    func markComplete(_ p: Persistence) throws {
        try p.db.run(
            PendingImport.table.filter(Columns.id == id).update(Columns.pending <- false)
        )
        logger.info("Marked pending import for recording \(recordingId) as complete")
    }
    
    private static func decode(row: Row) -> PendingImport? {
        do {
            let i: PendingImport = try row.decode()
            return i
        } catch {
            logger.error("Failed to decode PendingImport: \(error)")
            return nil
        }
    }
}
