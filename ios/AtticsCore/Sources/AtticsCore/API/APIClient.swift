//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/12/23.
//

import Foundation
import Combine

enum APIResult<T: Decodable> {
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


struct APIClient {
    let urlSession: URLSession = .shared
    static let localhost = "http://localhost:3000/api/"
    
    let baseUrl = APIResource(base: localhost)
    
    func getBands() -> AnyPublisher<[APIResources.BandWithMetadata], Error> {
        let url = baseUrl.appendingPath("bands.json")
        return get(url: url.toURL())
    }
    
    private func get<T: Decodable>(url: URL, decoder: JSONDecoder = defaultDecoder) -> AnyPublisher<T, Error> {
        print("APIClient: Getting \(url.absoluteString)")
        var request = URLRequest(url: url)
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        return urlSession.dataTaskPublisher(for: request)
            .map(\.data)
            .decode(type: T.self, decoder: decoder)
            .eraseToAnyPublisher()
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

