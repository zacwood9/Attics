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
    
    enum CacheStatus {
        case full
        case empty
        case expired
    }
    
    func cacheDate() -> Date? {
        return UserDefaults.standard.object(forKey: "cacheDate") as? Date
    }
    
    func updateCacheDate(to date: Date = Date()) {
        let defaults = UserDefaults.standard
        defaults.set(date, forKey: "cacheDate")
    }
    
    var status: CacheStatus {
        guard let cacheDate = cacheDate() else { return .empty }
        let lastWeekDate = Calendar.current.date(byAdding: .weekday, value: -2, to: Date())!
        if lastWeekDate > cacheDate { return .expired }
        return .full
    }
    
    func clearCache() {
        let fr = YearMO.fetchRequest() as NSFetchRequest<YearMO>
        let yearMOs = try! context.fetch(fr)
        for year in yearMOs {
            for show in year.shows?.allObjects as! [ShowMO] {
                context.delete(show)
            }
            context.delete(year)
        }
        
        try? context.save()
    }
    
    func updateCache(years: [YearWithTopShows]) {
        clearCache()
        
        for pair in years {
            let yearMO = YearMO(pair.year, into: context)
            for show in pair.shows {
                let _ = ShowMO(show, for: yearMO, into: context)
            }
        }

        do {
            try context.save()
            updateCacheDate()
            print("updated cache \(cacheDate()!)")
        } catch(let error) {
            print(error)
        }
    }
    
    func fetchTopShows(numShows: Int, then completion: @escaping (Result<[YearWithTopShows]>) -> ()) {
        let fr = YearMO.fetchRequest() as NSFetchRequest<YearMO>
        fr.sortDescriptors = [NSSortDescriptor(key: "year", ascending: true)]
        let yearMOs = try! context.fetch(fr)
        
        let res: [YearWithTopShows] = yearMOs.map { yearMO in
            let showMOs = yearMO.shows!.allObjects as! [ShowMO]
            let year = Year(yearMO: yearMO)
            let shows = showMOs.map { Show($0, year) }.sorted { $0.stars > $1.stars }
            return (year: year, shows: shows)
        }
        print("read from cache")
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
