//
//  AtticsTests.swift
//  AtticsTests
//
//  Created by Zachary Wood on 6/13/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import XCTest
@testable import Attics

class YearsViewControllerTests: XCTestCase {
    var yearsVC: YearsViewController!
    var dataStore: DataStore!
    
    override func setUp() {
        super.setUp()
        dataStore = InMemoryDataStore()
        yearsVC = YearsViewController(dataStore: dataStore)
        yearsVC.years.append(Year(id: 1, year: 1977))
    }
    
    override func tearDown() {
        super.tearDown()
    }
    
    func testTableView() {
        XCTAssertEqual(yearsVC.numberOfSections(in: yearsVC.tableView), 1)
        XCTAssertEqual(yearsVC.tableView(yearsVC.tableView, numberOfRowsInSection: 0), yearsVC.years.count)
        
        let firstCell = yearsVC.tableView(yearsVC.tableView, cellForRowAt: IndexPath(row: 0, section: 0))
        XCTAssertEqual(firstCell.textLabel?.text, "1977")
    }
}
