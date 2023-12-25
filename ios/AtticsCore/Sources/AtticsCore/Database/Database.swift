//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/12/23.
//

import Foundation
import SQLite

class Database {
    let connection: Connection
    
    struct Bands {
        static let table = Table("bands")
        
        static let id = Expression<String>("id")
        static let collection = Expression<String>("collection")
        static let name = Expression<String>("name")
        static let logoUrl = Expression<String?>("logo_url")
    }
    
    public init() throws {
        self.connection = try Connection(.inMemory)
    }
    
    func initializeSchema() throws {
        try connection.run(Bands.table.create { t in
            t.column(Bands.id, primaryKey: true)
            t.column(Bands.collection)
            t.column(Bands.name)
            t.column(Bands.logoUrl)
        })
    }
    
    func getBands() throws -> Array<String> {
        let rows = try connection.prepare(Bands.table)
        
        return rows.map { row in
            row[Bands.id]
        }
    }
}
