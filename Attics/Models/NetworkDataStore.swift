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

    }
    
    func fetchTopShows(then completion: @escaping (Result<[YearWithTopShows]>) -> ()) {
        WebApiService().load(NetworkYear.allWithTopShows) { result in
            switch result {
            case .success(let networkYears):
                let result: [YearWithTopShows] = networkYears.map { networkYear in
                    let year = Year(from: networkYear)
                    self.years.append(year)
                    
                    let shows = networkYear.topShows.map { topShow in
                        return Show(year: year, networkShow: topShow)
                    }
                    
                    return (year, shows)
                }
                
                completion(.success(result))
                
            case .failure(let error):
                completion(.failure(error))
                
            }
            
        }
    }
    
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ()) {
        WebApiService().load(year.shows) { result in
            switch result {
            case .success(let networkShows):
                print(networkShows)
                let shows: [Show] = networkShows.map { show in
                    let yearWithId = self.years.filter { year in
                        year.id == show.yearId
                    }
                    guard let first = yearWithId.first else { fatalError() }
                    return Show(year: first, networkShow: show)
                }
                
                completion(.success(shows))
                
            case .failure(let error):
                completion(.failure(error))
                
            }
        }
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
//        WebApiService().load(show.sources, then: completion)
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
//        WebApiService().load(source.songs, then: completion)
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
    let id: Int
    let date: String
    let venue: String?
    let city: String?
    let state: String?
    let stars: Double
    let sources: Int
    let avgRating: Double?
    let yearId: Int
}

fileprivate extension Year {
    init(from networkYear: NetworkYear) {
        id = networkYear.id
        year = networkYear.year
    }
    
    var shows: Resource<[NetworkShow]> {
        return Resource(url: URL(string: "http://localhost:3000/api/years/\(id)/shows")!, parse: parseJson)
    }
}

fileprivate extension Show {
    init(year: Year, networkShow: NetworkShow) {
        self.year = year
        self.id = networkShow.id
        self.date = networkShow.date
        self.venue = networkShow.venue ?? "Unknown"
        self.city = networkShow.city ?? "Unknown"
        self.state = networkShow.state ?? "Unknown"
        self.stars = networkShow.stars
        self.sources = networkShow.sources
        self.avgRating = networkShow.avgRating ?? 0
    }
}


