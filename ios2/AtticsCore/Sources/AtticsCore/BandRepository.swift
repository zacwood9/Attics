//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/23/23.
//

import Foundation
import SQLite

public struct Band: Codable {
    public let id: String
    public let collection: String
    public let name: String
}

extension Band {
    static func fromApi(apiBand: APIBand) -> Self {
        self.init(id: apiBand.id, collection: apiBand.collection, name: apiBand.name)
    }
}

public class BandRepository {
    let db: Connection
    
    static let table = Table("bands")
    var table: Table { BandRepository.table }
    struct Rows {
        static let id = Expression<String>("id")
        static let collection = Expression<String>("collection")
        static let name = Expression<String>("name")
    }
    
    public init(db: Connection) {
        self.db = db
    }
    
    public func getIds(_ ids: [String]) throws -> [Band] {
        return try db.prepare(table.filter(ids.contains(Rows.id))).compactMap {
            do {
                let band: Band = try $0.decode()
                return band
            } catch {
                print(error)
                return nil
            }
        }
    }
    
    @discardableResult
    public func upsertApi(_ apiBand: APIBand) throws -> Band {
        let band = Band.fromApi(apiBand: apiBand)
        try db.run(table.upsert(band, onConflictOf: Rows.id))
        return band
    }
    
    public func upsert(band: BandWithMetadata) throws -> Band {
        let newBand = Band(id: band.id, collection: band.collection, name: band.name)
        try db.run(table.upsert(newBand, onConflictOf: Rows.id))
        return newBand
    }
    
    public func loadSchema() throws {
        try db.run(BandRepository.table.create(ifNotExists: true) { t in
            t.column(Rows.id, primaryKey: true)
            t.column(Rows.name, unique: true)
            t.column(Rows.collection, unique: true)
        })
    }
}
