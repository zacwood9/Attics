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
        try! context.save()
    }
    
    static func findOrCreate(from year: Year, in context: NSManagedObjectContext) -> YearMO {
        let fr = fetchRequest() as NSFetchRequest<YearMO>
        fr.predicate = NSPredicate(format: "id = %d", Int64(year.id))
        do {
            let result = try context.fetch(fr)
            if let year = result.first {
                return year
            }
        } catch {
            print("YearMO.findOrCreate() fetch request failed: \(error)")
        }
        print("creating year")
        return YearMO.init(year, into: context)
    }
}
