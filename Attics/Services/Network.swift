//
//  File.swift
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
    func load<T>(_ resource: Resource<T>, completion: @escaping (Result<T>) -> ())
}

final class WebApiService: ApiService {
    func load<T>(_ resource: Resource<T>,  completion: @escaping (Result<T>) -> ()) {
        URLSession.shared.dataTask(with: resource.url) { (data, _, _) in
            guard let data = data else {
                completion(.failure(NetworkError(message: "Failed to load data.")))
                return
            }
            completion(resource.parse(data))
        }.resume()
    }
}

fileprivate let apiRoot = "https://stunning-yellowstone-51900.herokuapp.com/api"


extension Year {
    static var all: Resource<[Year]> {
        return Resource<[Year]>(url: URL(string: apiRoot + "/years")!) { data in
            do {
                let years = try JSONDecoder().decode([Year].self, from: data)
                return .success(years)
            } catch {
                print(error)
                return .failure(NetworkError(message: error.localizedDescription))
            }
        }

    }
}
