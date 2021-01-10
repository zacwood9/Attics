//
//  WebAPIClient.swift
//  Attics
//
//  Created by Zachary Wood on 7/6/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

final class WebAPIClient: APIClient {
    var years: [Year] = []
    var shows: [Show] = []
    var sources: [Source] = []
    
    func fetchTopShows(for band: Band, numShows: Int = 5, then completion: @escaping (APIResult<[YearWithTopShows]>) -> ()) {
        WebAPIService().load(band.topShows(numShows: numShows)) { result in
            switch result {
            case .success(let networkYears):
                // iterate over dictionary
                let result: [YearWithTopShows] = networkYears.sorted(by: {
                    $0.key < $1.key
                }).map { (key: String, value: [NetworkShow]) in
                    let year = Year(collection: band.collection, year: key)
                    self.years.append(year)
                    
                    let newShows = value.map { topShow in
                        return Show(year: year, networkShow: topShow)
                    }
                    
                    return (year, newShows)
                }
                completion(.success(result))
                
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
    
    func fetchShows(in year: Year, then completion: @escaping (APIResult<[Show]>) -> ()) {
        WebAPIService().load(year.shows) { result in
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
    
    func fetchSources(for show: Show?, then completion: @escaping (APIResult<[Source]>) -> ()) {
        guard let show = show else {
            completion(.failure(NetworkError(message: "NetworkDataStore.fetchSources -- No show")))
            return
        }
        
        WebAPIService().load(show.sourcesResource) { result in
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
    
    func fetchSongs(in source: Source, then completion: @escaping (APIResult<[Song]>) -> ()) {
        WebAPIService().load(source.songs) { result in
            switch result {
            case .success(let networkSongs):
                let songs = networkSongs.map { networkSong in
                    return Song(source: source, networkSong: networkSong)
                }
                
                completion(.success(songs))
                
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
}

let apiUrl = "https://attics.zacwood.me/v1.1"
//public let apiUrl = "http://localhost:8080/v1.1"

extension Band {
    func topShows(numShows: Int = 5) -> Resource<Dictionary<String, [NetworkShow]>> {
        return Resource(url: URL(string: "\(apiUrl)/\(collection)/top_shows?n=\(numShows)")!)
    }
    
    static var all: Resource<[Band]> {
        return Resource(url: URL(string: "\(apiUrl)/bands")!)
    }
}

struct NetworkYear: Codable {
    let id: Int
    let year: String
    let topShows: [NetworkShow]
}

extension Year {
    static func topShows(_ numShows: Int = 5) -> Resource<Dictionary<String, [NetworkShow]>> {
        return Resource(url: URL(string: "\(apiUrl)/DeadAndCompany/top_shows?n=\(numShows)")!)
    }
    
    var shows: Resource<[NetworkShow]> {
        return Resource(url: URL(string: "\(apiUrl)/\(collection)/\(year)")!)
    }
}

struct NetworkShow: Codable {
    let date: String
    let collection: String
    let venue: String?
    let city: String?
    let state: String?
    let avgRating: Double?
    let numReviews: Int?
    let numSources: Int?
    let yearId: Int?
}


fileprivate extension Show {
    init(year: Year, networkShow: NetworkShow) {
        self.year = year
        self.collection = networkShow.collection
        self.date = networkShow.date
        self.venue = networkShow.venue ?? "Unknown"
        self.city = networkShow.city ?? "Unknown"
        self.state = networkShow.state ?? "Unknown"
        self.numReviews = networkShow.numReviews ?? 0
        self.numSources = networkShow.numSources ?? 0
        self.avgRating = networkShow.avgRating ?? 0
    }
    
    var sourcesResource: Resource<[NetworkSource]> {
        return Resource(url: URL(string: "\(apiUrl)/\(collection)/\(year.year)/\(date)")!)
    }
}

struct NetworkSource: Codable {
    let identifier: String
    let transferer: String?
    let source: String?
    let avgRating: Double?
    let downloads: Int?
    let numReviews: Int?
    let lineage: String?
    let showId: Int
}

fileprivate extension Source {
    init(show: Show, networkSource: NetworkSource) {
        identifier = networkSource.identifier
        if networkSource.transferer != nil && networkSource.transferer! != "" {
            transferer = networkSource.transferer!
        } else {
            transferer = "Unknown"
        }
        if networkSource.source != nil && networkSource.source! != "" {
            source = networkSource.source!
        } else {
            source = "Unknown"
        }
        avgRating = networkSource.avgRating ?? 0
        downloads = networkSource.downloads ?? 0
        numReviews = networkSource.numReviews ?? 0
        if networkSource.lineage != nil && networkSource.lineage! != "" {
            lineage = networkSource.lineage!
        } else {
            lineage = "Unknown Lineage"
        }
        isFavorite = false
        self.show = show
        self.collection = show.collection
    }
    
    var songs: Resource<[NetworkSong]> {
        return Resource(url: URL(string: "\(apiUrl)/sources/\(identifier)")!)
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
        
        let t = Int(networkSong.track) ?? 0
        track = t
        if networkSong.title == nil || networkSong.title == "" {
            title = networkSong.fileName ?? "Track \(t)"
        } else {
            title = networkSong.title!
        }
        fileName = (networkSong.fileName == nil || networkSong.fileName == "") ? title : networkSong.fileName!
        album = networkSong.album ?? "\(source.show.date) - \(source.show.venue)"
        
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
