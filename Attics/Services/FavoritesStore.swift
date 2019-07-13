//
//  FavoritesStore.swift
//  Attics
//
//  Created by Zachary Wood on 7/10/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import UIKit
import CoreData

protocol FavoritesStore {
    func saveFavorite(source: Source)
    func removeFavorite(source: Source)
    func loadFavorites() -> [Source]
    func isFavorite(source: Source) -> Bool
}

// An implementation of FavoritesStore using CoreData.
struct CDFavoritesStore: FavoritesStore {
    var context: NSManagedObjectContext {
        return (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext
    }
    
    func saveFavorite(source: Source) {
        let mo = SourceMO.findOrCreate(from: source, in: context)
        mo.isFavorite = true
        do {
            try context.save()
        } catch {
            print("CDFavoritesStore.saveFavorite(): \(error)")
        }
        NotificationCenter.default.post(name: .FavoriteWasAdded, object: source.identifier)
    }
    
    func removeFavorite(source: Source) {
        let mo = SourceMO.findOrCreate(from: source, in: context)
        mo.isFavorite = false
        do {
            try context.save()
        } catch {
            print("CDFavoritesStore.removeFavorite(): \(error)")
        }
        NotificationCenter.default.post(name: .FavoriteWasRemoved, object: source.identifier)
    }
    
    func loadFavorites() -> [Source] {
        let fr = SourceMO.fetchRequest() as NSFetchRequest<SourceMO>
        fr.predicate = NSPredicate(format: "isFavorite = YES")
        if let result = try? context.fetch(fr) {
            return result.map(Source.init)
        }
        return []
    }
    
    func isFavorite(source: Source) -> Bool {
        let mo = SourceMO.findOrCreate(from: source, in: context)
        return mo.isFavorite
    }
}

extension CDFavoritesStore: SourcesDataStore {
    func fetchSources(for show: Show?, then completion: @escaping (Result<[Source]>) -> ()) {
        completion(.success(loadFavorites()))
    }
}

// An implementation of FavoritesStore using UserDefaults. I'm sorry
struct UDFavoritesStore: FavoritesStore {
        
    func saveFavorite(source: Source) {
        var favs = loadFavorites()
        favs.append(source)
        setFavorites(to: favs)
        NotificationCenter.default.post(name: .FavoriteWasAdded, object: source.identifier)
    }
    
    func removeFavorite(source: Source) {
        setFavorites(to: loadFavorites().filter { $0.identifier != source.identifier })
        NotificationCenter.default.post(name: .FavoriteWasRemoved, object: source.identifier)
    }
    
    func loadFavorites() -> [Source] {
        guard let data = UserDefaults.standard.data(forKey: "favorites") else { return [] }
        guard let favs = try? JSONDecoder().decode([Source].self, from: data) else { return [] }
        return favs
    }
    
    func isFavorite(source: Source) -> Bool {
        return loadFavorites().filter { $0.identifier == source.identifier }.count > 0
    }
    
    private func setFavorites(to sources: [Source]) {
        guard let data = try? JSONEncoder().encode(sources) else { return }
        UserDefaults.standard.set(data, forKey: "favorites")
    }
}

extension UDFavoritesStore: SourcesDataStore {
    func fetchSources(for show: Show?, then completion: @escaping (Result<[Source]>) -> ()) {
        completion(.success(loadFavorites()))
    }
}

extension Notification.Name {
    public static let FavoriteWasAdded = Notification.Name("FavoriteWasAdded")
    public static let FavoriteWasRemoved = Notification.Name("FavoriteWasRemoved")
}
