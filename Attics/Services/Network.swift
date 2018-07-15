//
//  Network.swift
//  Attics
//
//  Created by Zachary Wood on 7/6/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

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
            print(resource.url)
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

fileprivate let apiRoot = "https://stunning-yellowstone-51900.herokuapp.com/api"

func parseJson<T: Decodable>(from data: Data) -> Result<T> {
    do {
        let decoder = JSONDecoder()
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        let decodedItem = try decoder.decode(T.self, from: data)
        return .success(decodedItem)
    } catch {
        print(error)
        return .failure(NetworkError(message: error.localizedDescription))
    }
}

func downloadUrl(source: Source, song: Song) -> URL {
    return URL(string: "https://archive.org/download/\(source.identifier)/\(song.fileName)")!
}
