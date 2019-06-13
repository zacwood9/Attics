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
    init(_ showMO: ShowMO, _ year: Year) {
        self.id = Int(showMO.id)
        self.date = showMO.date!
        self.venue = showMO.venue!
        self.city = showMO.city!
        self.state = showMO.state!
        self.stars = showMO.stars
        self.sources = Int(showMO.sources)
        self.avgRating = showMO.avgRating
        self.year = year
    }
}

extension ShowMO {
    convenience init(_ show: Show, for year: YearMO, into context: NSManagedObjectContext) {
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
        self.year = year
    }
}
