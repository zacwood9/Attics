//
//  Show.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import CoreData

struct Show: Codable, Equatable {
    let id: Int
    let date: String
    let venue: String
    let city: String
    let state: String
    let stars: Double
    let sources: Int
    let avgRating: Double
    let year: Year
}

extension Show {
    init(_ showMO: ShowMO) {
        self.id = Int(showMO.id)
        self.date = showMO.date!
        self.venue = showMO.venue!
        self.city = showMO.city!
        self.state = showMO.state!
        self.stars = showMO.stars
        self.sources = Int(showMO.sources)
        self.avgRating = showMO.avgRating
        if let yearMO = showMO.year {
            self.year = Year(yearMO: yearMO)
        } else {
            fatalError("Show.init(): Show managed object does not belong to a year")
        }
    }
}

extension ShowMO {
    convenience init(_ show: Show, into context: NSManagedObjectContext) {
        let showED = NSEntityDescription.entity(forEntityName: "Show", in: context)!
        self.init(entity: showED, insertInto: context)
        self.id = Int64(show.id)
        self.date = show.date
        self.venue = show.venue
        self.city = show.city
        self.state = show.state
        self.stars = show.stars
        self.sources = Int64(show.sources)
        self.avgRating = show.avgRating
        self.year = YearMO.findOrCreate(from: show.year, in: context)
        try! context.save()
    }
    
    static func findOrCreate(from show: Show, in context: NSManagedObjectContext) -> ShowMO {
        let fr = fetchRequest() as NSFetchRequest<ShowMO>
        fr.predicate = NSPredicate(format: "id = %d", Int64(show.id))
        do {
            let result = try context.fetch(fr)
            if let show = result.first {
                return show
            }
        } catch {
            print("ShowMO.findOrCreate(): fetch request failed: \(error)")
        }
        return ShowMO.init(show, into: context)
    }
}
