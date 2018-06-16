//
//  InMemoryDataStoreTests.swift
//  AtticsTests
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import XCTest
@testable import Attics

class InMemoryDataStoreTests: XCTestCase {
    var dataStore: DataStore!
    
    override func setUp() {
        super.setUp()
        dataStore = InMemoryDataStore()
    }
    
    func testCorrectYears() {
        let fetchedYears = dataStore.fetchYears()
        let intendedYears = [Year(id: 1, year: 1977), Year(id: 2, year: 1978)]
        XCTAssertEqual(fetchedYears, intendedYears)
    }
    
    func testFetchShows() {
        let years = dataStore.fetchYears()
        let shows = dataStore.fetchShows(in: years[0])
        XCTAssertEqual(shows.count, 2)
        XCTAssertEqual(shows[0].date, "1977-05-07")
        XCTAssertEqual(shows[0].year, years[0])
    }
    
    func testEmptyFetch() {
        let fakeYear = Year(id: -1, year: 0)
        let noShows = dataStore.fetchShows(in: fakeYear)
        XCTAssert(noShows.count == 0)
    }
    
    func testFetchSources() {
        let years = dataStore.fetchYears()
        let shows = dataStore.fetchShows(in: years[0])
        let sources = dataStore.fetchSources(for: shows[1])
        
        XCTAssertEqual(sources.count, 1)
        XCTAssertEqual(sources[0].transferer, "Rob Eaton")
        
        let otherShows = dataStore.fetchShows(in: years[1])
        let otherSources = dataStore.fetchSources(for: otherShows[0])
        XCTAssertEqual(otherSources.count, 2)
        XCTAssertEqual(otherSources[1].transferer, "Bob Marley")
        
        
    }
}
