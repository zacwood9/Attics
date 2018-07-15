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
    let songs: [Song]
    
    func fetchYears(then completion: @escaping (Result<[NetworkYear]>) -> ()) {
        completion(.success([]))
    }
    
    func fetchTopShows(then completion: @escaping (Result<[YearWithTopShows]>) -> ()) {
        completion(.success([]))
    }
    
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ()) {
        completion(.success([]))
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
        completion(.success([]))
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
        completion(.success([]))
    }
    
    init() {
        years = [Year(id: 1, year: "1977"), Year(id: 2, year: "1978")]
        shows = [
//            Show.init(id: 1, date: "1977-05-07", venue: "Boston", yearId: years[0].id),
//            Show.init(id: 2, date: "1977-05-08", venue: "Barton Hall", yearId: years[0].id),
//            Show.init(id: 3, date: "1978-01-01", venue: "Venue 1", yearId: years[1].id),
        ]
        sources = [
//            Source(id: 1, transferer: "Rob Eaton", identifier: "gd77-05-08.sbd.hicks.4982.sbeok.shnf", showId: shows[1].id),
//            Source(id: 2, transferer: "Bob Weir", identifier: "gd2", showId: shows[2].id),
//            Source(id: 3, transferer: "Bob Marley", identifier: "gd3", showId: shows[2].id)
        ]
        songs = [
//            Song(title: "Minglewood Blues", fileName: "gd77-05-08eaton-d1t01.mp3"),
//            Song(title: "Loser", fileName: "gd77-05-08eaton-d1t02.mp3")
        ]
    }
}
