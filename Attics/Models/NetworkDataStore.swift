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
    
    func fetchYears(then completion: @escaping (Result<[NetworkYear]>) -> ()) {
//        WebApiService().load(NetworkYear.allWithTopShows) { result in
//            switch result {
//            case .success(let networkYears):
//                print(networkYears)
//            case .failure(let error):
//                completion(.failure(error))
//            }
//        }
        
        WebApiService().load(NetworkYear.allWithTopShows, then: completion)
    }
    
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ()) {
        WebApiService().load(year.shows, then: completion)
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
        WebApiService().load(show.sources, then: completion)
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
        WebApiService().load(source.songs, then: completion)
    }
}

struct NetworkYear: Codable {
    let id: Int
    let year: String
    let topShows: [NetworkShow]
}

extension NetworkYear {
    static var allWithTopShows: Resource<[NetworkYear]> {
        return Resource(url: URL(string: "http://localhost:3000/api/years")!, parse: parseJson)
    }
}

struct NetworkShow: Codable {
    let date: String
    let venue: String
    let city: String
    let state: String
    let stars: Double
    let sources: Int
    let avgRating: Double
}


