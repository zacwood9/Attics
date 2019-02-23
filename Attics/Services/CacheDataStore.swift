//
//  CacheDataStore.swift
//  Attics
//
//  Created by Zachary Wood on 2/22/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import CoreData

class CacheDataStore : DataStore {
    var context: NSManagedObjectContext
    
    func fetchTopShows(numShows: Int, then completion: @escaping (Result<[YearWithTopShows]>) -> ()) {
        let fr = YearMO.fetchRequest() as NSFetchRequest<YearMO>
        fr.sortDescriptors = [NSSortDescriptor(key: "year", ascending: true)]
        let yearMOs = try! context.fetch(fr)
        
        let res: [YearWithTopShows] = yearMOs.map { yearMO in
            let showMOs = yearMO.shows!.allObjects as! [ShowMO]
            let year = Year(yearMO: yearMO)
            let shows = showMOs.map { Show($0, year) }
            return (year: year, shows: shows)
        }
        
        completion(.success(res))
    }
    
    func fetchShows(in year: Year, then completion: @escaping (Result<[Show]>) -> ()) {
        
    }
    
    func fetchSources(for show: Show, then completion: @escaping (Result<[Source]>) -> ()) {
        
    }
    
    func fetchSongs(in source: Source, then completion: @escaping (Result<[Song]>) -> ()) {
        
    }
    
    func fetchReviews(for source: Source, then completion: @escaping (Result<[Review]>) -> ()) {
        
    }
    
    init(context: NSManagedObjectContext) {
        self.context = context
    }
}
