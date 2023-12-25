//
//  File.swift
//
//
//  Created by Zachary Wood on 12/23/23.
//

import Foundation
import SQLite

public struct Performance: Codable {
    public let id: String
    public let date: String
    public let venue: String
    public let city: String
    public let state: String
    public let bandId: String
}

extension Performance {
    public static func fromApi(apiPerformance: APIPerformance) -> Self {
        self.init(id: apiPerformance.id, date: apiPerformance.date, venue: apiPerformance.venue, city: apiPerformance.city, state: apiPerformance.state, bandId: apiPerformance.bandId)
    }
}

public class PerformanceRepository {
    let db: Connection
    
    static let table = Table("performances")
    var table: Table { PerformanceRepository.table }
    
    struct Rows {
        static let id = Expression<String>("id")
        static let date = Expression<String>("date")
        static let venue = Expression<String>("venue")
        static let city = Expression<String>("city")
        static let state = Expression<String>("state")
        static let bandId = Expression<String>("bandId")
    }
    
    public init(db: Connection) {
        self.db = db
    }
    
    public func getIds(_ ids: [String]) throws -> [Performance] {
        return try db.prepare(table.filter(ids.contains(Rows.id))).compactMap {
            do {
                let performance: Performance = try $0.decode()
                return performance
            } catch {
                print(error)
                return nil
            }
        }
    }
    
    @discardableResult
    public func upsertApi(_ apiPerformance: APIPerformance) throws -> Performance {
        let performance = Performance.fromApi(apiPerformance: apiPerformance)
        try db.run(table.upsert(performance, onConflictOf: Rows.id))
        return performance
    }
    
    public func loadSchema() throws {
        try db.run(table.create(ifNotExists: true) { t in
            t.column(Rows.id, primaryKey: true)
            t.column(Rows.date)
            t.column(Rows.venue)
            t.column(Rows.city)
            t.column(Rows.state)
            t.column(Rows.bandId)
            
            t.foreignKey(Rows.bandId, references: BandRepository.table, BandRepository.Rows.id)
        })
    }
}
