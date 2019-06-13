//
//  Year.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import CoreData

struct Year: Codable, Equatable {
    let id: Int
    let year: String
}

extension Year {
    init(yearMO: YearMO) {
        self.id = Int(yearMO.id)
        self.year = yearMO.year!
    }
}

extension YearMO {
    convenience init(_ year: Year, into context: NSManagedObjectContext) {
        let yearED = NSEntityDescription.entity(forEntityName: "Year", in: context)!
        self.init(entity: yearED, insertInto: context)
        self.id = Int64(year.id)
        self.year = year.year        
    }
}
