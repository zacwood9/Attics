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
    var shows: [Show] = []
    var sources: [Source] = []
    
    func fetchTopShows(numShows: Int = 5, then completion: @escaping (Result<[YearWithTopShows]>) -> ()) {
        WebApiService().load(Year.allWithTopShows(numShows: numShows)) { result in
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
                let shows: [Show] = networkShows.map { show in
                    let show = Show(year: year, networkShow: show)
                    if !self.shows.contains(show) { self.shows.append(show) }
                    
                    return show
                }
                
                completion(.success(shows))
                
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
        WebApiService().load(show.sourcesResource) { result in
            switch result {
            case .success(let networkSources):
                let sources: [Source] = networkSources.map { networkSource in
                    let source = Source(show: show, networkSource: networkSource)
                    if !self.sources.contains(source) { self.sources.append(source) }
                    
                    return source
                }
                
                completion(.success(sources))
                
            case .failure(let error):
                completion(.failure(error))
                
            }
        }
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
        WebApiService().load(source.songs) { result in
            switch result {
            case .success(let networkSongs):
                let songs: [Song] = networkSongs.map { networkSong in
                    return Song(source: source, networkSong: networkSong)                    
                }
                
                completion(.success(songs))
                
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
    
    func fetchReviews(for source: Source, then completion: @escaping ((Result<[Review]>) -> ())) {
        WebApiService().load(source.reviews) { result in
            switch result {
            case .success(let networkReviews):
                let reviews: [Review] = networkReviews.map(Review.init)
                completion(.success(reviews))
                
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
}

fileprivate let apiUrl = "https://gdapi.zacwood.me/api"

struct NetworkYear: Codable {
    let id: Int
    let year: String
    let topShows: [NetworkShow]
}

fileprivate extension Year {
    init(from networkYear: NetworkYear) {
        id = networkYear.id
        year = networkYear.year
    }
    
    var shows: Resource<[NetworkShow]> {
        return Resource(url: URL(string: "\(apiUrl)/years/\(id)/shows")!, parse: parseJson)
    }
    
    static func allWithTopShows(numShows: Int = 5) -> Resource<[NetworkYear]> {
        return Resource(url: URL(string: "\(apiUrl)/years?shows_per_year=\(numShows)")!, parse: parseJson)
    }
}

struct NetworkShow: Codable {
    let id: Int
    let date: String
    let venue: String?
    let city: String?
    let state: String?
    let stars: Double?
    let sources: Int?
    let avgRating: Double?
    let yearId: Int
}


fileprivate extension Show {
    init(year: Year, networkShow: NetworkShow) {
        self.year = year
        self.id = networkShow.id
        self.date = networkShow.date
        self.venue = networkShow.venue ?? "Unknown"
        self.city = networkShow.city ?? "Unknown"
        self.state = networkShow.state ?? "Unknown"
        self.stars = networkShow.stars ?? 0
        self.sources = networkShow.sources ?? 0
        self.avgRating = networkShow.avgRating ?? 0
    }
    
    var sourcesResource: Resource<[NetworkSource]> {
        return Resource(url: URL(string: "\(apiUrl)/shows/\(id)/sources")!, parse: parseJson)
    }
}

struct NetworkSource: Codable {
    let id: Int
    let identifier: String
    let transferer: String?
    let source: String?
    let avgRating: Double?
    let downloads: Int?
    let numReviews: Int?
    let description: String?
    let lineage: String?
    let showId: Int
}

fileprivate extension Source {
    init(show: Show, networkSource: NetworkSource) {
        id = networkSource.id
        identifier = networkSource.identifier
        transferer = networkSource.transferer ?? "Unknown"
        source = networkSource.source ?? "Unknown"
        avgRating = networkSource.avgRating ?? 0
        downloads = networkSource.downloads ?? 0
        numReviews = networkSource.numReviews ?? 0
        description = networkSource.description ?? "Unknown"
        lineage = networkSource.lineage ?? "Unknown Lineage"
        self.show = show
    }
    
    var songs: Resource<[NetworkSong]> {
        return Resource(url: URL(string: "\(apiUrl)/sources/\(id)/songs")!, parse: parseJson)
    }
    
    var reviews: Resource<[NetworkReview]> {
        return Resource(url: URL(string: "\(apiUrl)/sources/\(id)/reviews")!, parse: parseJson)
    }
}

struct NetworkSong: Codable {
    let title: String?
    let fileName: String?
    let album: String?
    let track: String
    let length: String?
    
    enum CodingKeys: String, CodingKey {
        case title, album, length, track
        case fileName = "name"
    }
}

fileprivate extension Song {
    init(source: Source, networkSong: NetworkSong) {
        self.source = source
        
        title = networkSong.title ?? networkSong.fileName ?? "unknown file name"
        fileName = networkSong.fileName ?? "unknown file name"
        album = networkSong.album ?? "\(source.show.date) - \(source.show.venue)" 
        track = Int(networkSong.track) ?? 0
        length = networkSong.length ?? "0:00"
    }
}

struct NetworkReview: Codable {
    let reviewbody: String
    let reviewtitle: String
    let reviewer: String
    let reviewdate: Date
    let stars: String
}

fileprivate extension Review {
    init(from networkReview: NetworkReview) {
        self.title = networkReview.reviewtitle
        self.author = networkReview.reviewer
        self.body = networkReview.reviewbody
        self.stars = Int(networkReview.stars) ?? 0
        self.date = networkReview.reviewdate
    }
}


