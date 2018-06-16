//
//  InMemoryDataStore.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

class InMemoryDataStore: DataStore {
    let years: [Year]
    let shows: [Show]
    let sources: [Source]
    
    func fetchYears() -> [Year] {
        return years
    }
    
    func fetchShows(in year: Year) -> [Show] {
        return shows.filter { $0.year == year }
    }
    
    func fetchSources(for show: Show) -> [Source] {
        return sources.filter { $0.show == show }
    }
    
    init() {
        years = [Year(id: 1, year: 1977), Year(id: 2, year: 1978)]
        shows = [
            Show.init(id: 1, date: "1977-05-07", venue: "Boston", year: years[0]),
            Show.init(id: 2, date: "1977-05-08", venue: "Barton Hall", year: years[0]),
            Show.init(id: 3, date: "1978-01-01", venue: "Venue 1", year: years[1]),
        ]
        sources = [
            Source(id: 1, transferer: "Rob Eaton", show: shows[1]),
            Source(id: 2, transferer: "Bob Weir", show: shows[2]),
            Source(id: 3, transferer: "Bob Marley", show: shows[2])
        ]
    }
}
