//
//  SongsViewControllerTests.swift
//  AtticsTests
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import XCTest
@testable import Attics

class SongsViewControllerTests: XCTestCase {
    var songsVC: SongsViewController!
    var dataStore: DataStore!
    var songs: [Song] = []
    override func setUp() {
        super.setUp()
        dataStore = InMemoryDataStore()
        
        let years = dataStore.fetchYears()
        let shows = dataStore.fetchShows(in: years[0])
        let sources = dataStore.fetchSources(for: shows[1])
        songs = dataStore.fetchSongs(for: sources[0])
        
        songsVC = SongsViewController(from: sources[0], dataStore: dataStore)
    }
    
    func testTableView() {
        let songTable = songsVC.tableView
        
        XCTAssertEqual(songsVC.tableView(songTable, numberOfRowsInSection: 0), 2)
    }
}
