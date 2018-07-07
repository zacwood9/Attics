//
//  DataStore.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import Foundation

protocol DataStore {
    func fetchYears() -> Result<[Year]>
    func fetchShows(in year: Year) -> [Show]
    func fetchSources(for show: Show) -> [Source]
    func fetchSongs(for source: Source) -> [Song]
}
