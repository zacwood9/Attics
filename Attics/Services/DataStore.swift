//
//  DataStore.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

typealias YearWithTopShows = (year: Year, shows: [Show])

protocol SourcesDataStore {
    func fetchSources(for show: Show?, then completion: @escaping (Result<[Source]>) -> ())
}

protocol SongsDataStore {
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ())
}

protocol DataStore: SongsDataStore {
    func fetchTopShows(numShows: Int, then completion: @escaping (Result<[YearWithTopShows]>) -> ())
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ())
    func fetchSources(for show: Show?, then completion: @escaping (Result<[Source]>) -> ())
    func fetchReviews(for source: Source, then completion: @escaping (Result<[Review]>) -> ())
}
