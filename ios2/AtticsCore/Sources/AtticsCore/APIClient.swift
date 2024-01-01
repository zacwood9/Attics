//
//  File.swift
//
//
//  Created by Zachary Wood on 12/12/23.
//

import Foundation
import Combine

public enum APIResult<T: Decodable> {
    case loading
    case success(T)
    case error(Error)
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


public struct APIClient {
    let urlSession: URLSession = .shared
    static let localhost = "http://localhost:3000/api/"
    static let production = "https://attics.io/api/"
    static let archiveReview = "https://archive.org/metadata/"
    
    let baseUrl = APIResource(base: production)
    let archiveReviewUrl = APIResource(base: archiveReview)
    
    public func getBands() async throws -> [BandWithMetadata] {
        let url = baseUrl.appendingPath("bands.json")
        return try await asyncGet(url: url.toURL())
    }
    
    public func getTopPerformances(bandId: String, perYear: Int = 5) async throws -> TopPerformancesPage {
        let formatter = ISO8601DateFormatter()
        formatter.timeZone = .current
        formatter.formatOptions = .withFullDate
        let formatted = formatter.string(from: Date())
        
        let url = baseUrl
            .appendingPath("bands")
            .appendingPath(bandId)
            .appendingPath("top_performances.json")
            .appendingQuery(key: "performances_per_year", value: "\(perYear)")
            .appendingQuery(key: "on_this_day", value: formatted)
        
        return try await asyncGet(url: url.toURL())
    }

    public func getYearPerformances(bandId: String, year: String) async throws -> [PerformanceWithMetadata] {
        let url = baseUrl.appendingPath("bands").appendingPath(bandId).appendingPath("years").appendingPath("\(year).json")
        return try await asyncGet(url: url.toURL())
    }
    
    public func getPerformance(performanceId: String) async throws -> [APIRecording] {
        let url = baseUrl.appendingPath("performances").appendingPath(performanceId)
        return try await asyncGet(url: url.toURL())
    }
    
    public func getRecording(recordingId: String) async throws -> APIRecordingPage {
        let url = baseUrl.appendingPath("recordings").appendingPath(recordingId)
        return try await asyncGet(url: url.toURL())
    }
    
    public func getRecording(identifier: String) async throws -> APIRecordingPage {
        let url = baseUrl.appendingPath("recordings").appendingPath(identifier)
        return try await asyncGet(url: url.toURL())
    }
    
    public func getReviews(archiveIdentifier: String) -> AnyPublisher<[APIReview], Error> {
        let url = archiveReviewUrl.appendingPath(archiveIdentifier).appendingPath("reviews")
        return get(url: url.toURL())
    }
    
    private func get<T: Decodable>(url: URL, decoder: JSONDecoder = defaultDecoder) -> AnyPublisher<T, Error> {
        logger.info("APIClient: Getting \(url.absoluteString)")
        var request = URLRequest(url: url)
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        return urlSession.dataTaskPublisher(for: request)
            .map(\.data)
            .decode(type: T.self, decoder: decoder)
            .eraseToAnyPublisher()
    }
    
    private func asyncGet<T: Decodable>(url: URL, decoder: JSONDecoder = defaultDecoder) async throws -> T {
        logger.info("APIClient: Getting \(url.absoluteString)")
        var request = URLRequest(url: url)
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        
        let (data, _) = try await urlSession.data(for: request)
        return try decoder.decode(T.self, from: data)
    }
    
    private static let defaultDecoder = {
        let decoder: JSONDecoder = .snakeCaseDecoder
        return decoder
    }()
}

extension JSONDecoder {
    static var snakeCaseDecoder: JSONDecoder {
        let decoder = JSONDecoder()
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        return decoder
    }
}

