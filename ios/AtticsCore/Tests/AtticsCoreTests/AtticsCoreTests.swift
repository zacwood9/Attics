import XCTest
@testable import AtticsCore

import SQLite

final class AtticsCoreTests: XCTestCase {
    func testExample() throws {
        let db = try Database()
        try db.initializeSchema()
        
        let id = UUID().uuidString
        let rowId = try db.connection.run(Database.Bands.table.insert([
            Database.Bands.id <- id,
            Database.Bands.name <- "Grateful Dead",
            Database.Bands.collection <- "GratefulDead",
            Database.Bands.logoUrl <- "https://google.com",
        ]))
        
        XCTAssertEqual(id, try db.getBands().first)
    }
}
