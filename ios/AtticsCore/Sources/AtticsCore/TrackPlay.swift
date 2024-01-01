//
//  History.swift
//
//
//  Created by Zachary Wood on 12/29/23.
//

import Foundation
import SQLite

struct TrackPlay : Codable {
    static let table = Table("track_plays")
    enum Columns {
        static let id = Expression<String>("id")
        static let trackId = Expression<String>("trackId")
        static let playedAt = Expression<Date>("playedAt")
    }
    
    let id: String
    let trackId: String
    let playedAt: Date
    
    var table: Table { Self.table }
    
    static func all(_ p: Persistence) throws -> [Self] {
        let rows = try p.db.prepare(table)
        return rows.compactMap { Self.decode(row: $0) }
    }
    
    @discardableResult
    static func create(_ p: Persistence, trackId: String) throws -> Self {
        let trackPlay = TrackPlay(id: UUID().uuidString, trackId: trackId, playedAt: Date())
        try p.db.run(table.insert(trackPlay))
        return trackPlay
    }
    
    private static func decode(row: Row) -> Self? {
        do {
            let i: Self = try row.decode()
            return i
        } catch {
            logger.error("Failed to decode TrackPlay: \(error)")
            return nil
        }
    }
}
