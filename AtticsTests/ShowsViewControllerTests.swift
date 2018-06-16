//
//  ShowsViewControllerTests.swift
//  AtticsTests
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import XCTest
@testable import Attics

class ShowsViewControllerTests: XCTestCase {
    var showsVC: ShowsViewController!
    var dataStore: DataStore!
    
    override func setUp() {
        super.setUp()
        dataStore = InMemoryDataStore()
        showsVC = ShowsViewController(withShowsIn: dataStore.fetchYears()[0], dataStore: dataStore)
    }
    
    override func tearDown() {
        super.tearDown()
    }
    
    func testTableView() {
        let showTable = showsVC.tableView        
        XCTAssertEqual(showsVC.tableView(showTable, numberOfRowsInSection: 0), 2)
        
        let firstCell = showsVC.tableView(showTable, cellForRowAt: IndexPath(row: 0, section: 0))
        XCTAssertEqual(firstCell.textLabel?.text, "1977-05-07")
    }
}
