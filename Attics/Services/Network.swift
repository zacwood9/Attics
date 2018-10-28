//
//  Network.swift
//  Attics
//
//  Created by Zachary Wood on 7/6/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

struct NetworkError: Error {
    let message: String
}

enum Result<T> {
    case success(T)
    case failure(NetworkError)
}

struct Resource<T> {
    let url: URL
    let parse: (Data) -> Result<T>
}

protocol ApiService {
    func load<T>(_ resource: Resource<T>, then completion: @escaping (Result<T>) -> ())
}

final class WebApiService: ApiService {
    let urlSession: URLSession
    
    func load<T>(_ resource: Resource<T>, then completion: @escaping (Result<T>) -> ()) {
        urlSession.dataTask(with: resource.url) { (data, _, _) in
            guard let data = data else {
                completion(.failure(NetworkError(message: "Failed to load data.")))
                return
            }
            completion(resource.parse(data))
        }.resume()
    }
    
    init(urlSession: URLSession = URLSession.shared) {
        self.urlSession = urlSession
    }
}

func parseJson<T: Decodable>(from data: Data) -> Result<T> {
    do {
        let decoder = JSONDecoder()
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        
        let decodedItem = try decoder.decode(T.self, from: data)
        return .success(decodedItem)
    } catch {
        return .failure(NetworkError(message: error.localizedDescription))
    }
}

extension Song {
    var downloadUrl: URL {
        return URL(string: "https://archive.org/download/\(source.identifier)/\(fileName)")!
    }
}
