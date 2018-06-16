//
//  SourcesViewControllerTests.swift
//  AtticsTests
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import XCTest
@testable import Attics

class SourcesViewControllerTests: XCTestCase {
    var sourcesVC: SourcesViewController!
    var dataStore: DataStore!
    
    override func setUp() {
        super.setUp()
        dataStore = InMemoryDataStore()
        let year = dataStore.fetchYears()[1] // 1978
        let show = dataStore.fetchShows(in: year)[0] // 01-01-1978
        sourcesVC = SourcesViewController(for: show, dataStore: dataStore)
    }
    
    override func tearDown() {
        super.tearDown()
    }
    
    func testTableView() {
        let sourcesTable = sourcesVC.tableView
        XCTAssertEqual(sourcesVC.tableView(sourcesTable, numberOfRowsInSection: 0), 2)
        
        let firstCell = sourcesVC.tableView(sourcesTable, cellForRowAt: IndexPath(row: 0, section: 0))
        XCTAssertEqual(firstCell.textLabel?.text, "Bob Weir")
    }
}
