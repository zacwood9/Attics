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
    
    func updateCacheDate(to date: Date) {
        let defaults = UserDefaults.standard
        defaults.set(date, forKey: "cacheDate")
    }
    
    var cacheStatus: CacheStatus {
        guard let cacheDate = cacheDate() else { return .empty }
        let lastWeekDate = Calendar.current.date(byAdding: .minute, value: -1, to: Date())!
        
        print(cacheDate.compare(lastWeekDate))
        
        let fr = YearMO.fetchRequest() as NSFetchRequest<YearMO>
        let yearMOs = try! context.fetch(fr)
        if yearMOs.isEmpty {
            return .empty
        }
        return .full
    }
    
//    func updateCache() {
//        for pair in years {
//            let yearMO = YearMO(pair.year, into: context)
//            for show in pair.shows {
//                let _ = ShowMO(show, for: yearMO, into: context)
//            }
//        }
//
//        do {
//            try context.save()
//        } catch(let error) {
//            print(error)
//        }
//    }
    
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

extension Date {
    func isInSameWeek(date: Date) -> Bool {
        return Calendar.current.isDate(self, equalTo: date, toGranularity: .weekOfYear)
    }
    func isInSameMonth(date: Date) -> Bool {
        return Calendar.current.isDate(self, equalTo: date, toGranularity: .month)
    }
    func isInSameYear(date: Date) -> Bool {
        return Calendar.current.isDate(self, equalTo: date, toGranularity: .year)
    }
    func isInSameDay(date: Date) -> Bool {
        return Calendar.current.isDate(self, equalTo: date, toGranularity: .day)
    }
    var isInThisWeek: Bool {
        return isInSameWeek(date: Date())
    }
    var isInToday: Bool {
        return Calendar.current.isDateInToday(self)
    }
    var isInTheFuture: Bool {
        return Date() < self
    }
    var isInThePast: Bool {
        return self < Date()
    }
}
