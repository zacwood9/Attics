//
//  NetworkDataStore.swift
//  Attics
//
//  Created by Zachary Wood on 7/6/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

final class NetworkDataStore: DataStore {
    var years: [Year] = []
    
    func fetchYears(then completion: @escaping (Result<[Year]>) -> ()) {
//        WebApiService().load(Year.all) { [unowned self] result in
//            switch result {
//            case .success(let years):
//                self.years = years
//                fallthrough
//            default:
//                completion(result)
//            }
//        }
        WebApiService().load(Year.all, then: completion)
    }
    
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ()) {
        WebApiService().load(year.shows, then: completion)
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
        WebApiService().load(show.sources, then: completion)
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
        completion(.success([]))
    }
}
