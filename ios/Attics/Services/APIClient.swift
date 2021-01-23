//
//  DataStore.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation
import Combine

typealias YearWithTopShows = (year: Year, shows: [Show])

protocol BandStore {
    var band: Band { get set }
}

struct APIError: Codable, Error {
    let message: String
    let code: Int
}

enum APIResult<T: Decodable> {
    case loading
    case success(T)
    case error(APIError)
}

struct APIResource {
    var base: String
    private var builder: String = ""
    private var queries: [(String, String)] = []
    
    init(base: String) {
        self.base = base
    }
    
    private init(base: String, builder: String, queries: [(String, String)]) {
        self.base = base
        self.builder = builder
        self.queries = queries
    }
    
    func toURL() -> URL {
        let q = queries.isEmpty ? "" : "?" + queries.map { $0.0 + "=" + $0.1 }.joined(separator: "&")
        return URL(string: base + builder + q)!
    }
    
    func appendingPath(_ path: String) -> APIResource {
        var b = self.builder
        if let last = builder.last, last != Character("/") {
            b += "/"
        }
        b += path
        return APIResource(base: base, builder: b, queries: queries)
    }
    
    func appendingQuery(key: String, value: String) -> APIResource {
        return APIResource(base: base, builder: builder, queries: queries + [(key, value)])
    }
}


struct APIClient {
    let urlSession: URLSession = .shared
    let baseUrl = APIResource(base: "https://attics.io/")
    
    func getBands() -> AnyPublisher<[BandWithMetadata], APIError> {
        let url = baseUrl.appendingPath("Bands")
        return get(url: url.toURL())
    }
    
    func getTopPerformances(_ band: Band) -> AnyPublisher<BandResponse, APIError> {
        let url = baseUrl
            .appendingPath("TopPerformances")
            .appendingQuery(key: "collection", value: band.collection)
            .appendingQuery(key: "numPerformances", value: "5")
        return get(url: url.toURL())
    }
    
    func getMigrationItems(_ identifiers: [String]) -> AnyPublisher<[MigrationItem], APIError> {
        let url = baseUrl
            .appendingPath("Migration")
            .appendingQuery(key: "identifiers", value: identifiers.joined(separator: ","))
        return get(url: url.toURL())
    }
    
    func getPerformances(for band: Band, in year: Year) -> AnyPublisher<YearResponse, APIError> {
        let url = baseUrl
            .appendingPath("ShowBand")
            .appendingQuery(key: "collection", value: band.collection)
            .appendingQuery(key: "year", value: year)
        
        return get(url: url.toURL())
    }
    
    func getShow(for band: Band, recordingsOf date: String) -> AnyPublisher<ShowResponse, APIError> {
        let url = baseUrl
            .appendingPath("Recordings")
            .appendingQuery(key: "collection", value: band.collection)
            .appendingQuery(key: "date", value: date)
        
        return get(url: url.toURL())
    }
    
    func getRecording(identifier: String) -> AnyPublisher<SourceResponse, APIError> {
        let url = baseUrl.appendingPath("ShowRecording")
            .appendingQuery(key: "identifier", value: identifier)
        
        return get(url: url.toURL())
    }
    
    private func get<T: Decodable>(url: URL, decoder: JSONDecoder = .snakeCaseDecoder) -> AnyPublisher<T, APIError> {
        print("APIClient: Getting \(url.absoluteString)")
        var request = URLRequest(url: url)
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        return urlSession.dataTaskPublisher(for: request)
            .map(\.data)
            .decode(type: T.self, decoder: decoder)
            .mapError { error in
                if let error = error as? APIError { return error }
                return APIError(message: error.localizedDescription, code: -1)
            }
            .eraseToAnyPublisher()
    }
}

struct MigrationItem : Decodable {
    let band: BandWithMetadata
    let performance: Show
    let recording: Source
}

extension JSONDecoder {
    static var snakeCaseDecoder: JSONDecoder {
        let decoder = JSONDecoder()
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        return decoder
    }
}
