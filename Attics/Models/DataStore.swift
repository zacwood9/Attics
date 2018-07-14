//
//  DataStore.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

protocol DataStore {
    func fetchYears(then completion: @escaping (Result<[NetworkYear]>) -> ())
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ())
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ())
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ())
}
