//
//  UITests.swift
//  UITests
//
//  Created by Zachary Wood on 1/4/21.
//  Copyright © 2021 Zachary Wood. All rights reserved.
//

import XCTest

class UITests: XCTestCase {

    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.

        // In UI tests it is usually best to stop immediately when a failure occurs.
        continueAfterFailure = false

        // In UI tests it’s important to set the initial state - such as interface orientation - required for your tests before they run. The setUp method is a good place to do this.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    func testExample() throws {
        // UI tests must launch the application that they test.
        
        let app = XCUIApplication()
        app.launch()
        app.tabBars["Tab Bar"].buttons["Browse"].tap()
        app.navigationBars.firstMatch.buttons["Change band"].tap()
        
        let tablesQuery = app.tables
        tablesQuery/*@START_MENU_TOKEN@*/.staticTexts["Billy Strings"]/*[[".cells.staticTexts[\"Billy Strings\"]",".staticTexts[\"Billy Strings\"]"],[[[-1,1],[-1,0]]],[0]]@END_MENU_TOKEN@*/.tap()
        tablesQuery/*@START_MENU_TOKEN@*/.staticTexts["2013-08-04"]/*[[".cells.staticTexts[\"2013-08-04\"]",".staticTexts[\"2013-08-04\"]"],[[[-1,1],[-1,0]]],[0]]@END_MENU_TOKEN@*/.tap()
        
        app.collectionViews.cells.firstMatch.tap()
    }

    func testLaunchPerformance() throws {
        if #available(macOS 10.15, iOS 13.0, tvOS 13.0, *) {
            // This measures how long it takes to launch your application.
            measure(metrics: [XCTApplicationLaunchMetric()]) {
                XCUIApplication().launch()
            }
        }
    }
}
