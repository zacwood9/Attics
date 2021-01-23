//
//  Migrations.swift
//  Attics
//
//  Created by Zachary Wood on 12/2/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//
//  When building Attics, things change. For example, moving from
//  CoreData to a FileSystem store. This file contains functions which migrate
//  from old versions to new versions to facilitate these changes.

import Foundation
import Combine

func runMigrations() {
    migrateToApplicationSupport()
    migrateToV2({}, {})
}

/// Migrates storage from Library/Caches to Library/Applicatiion Support.
/// Caches folder can be wiped by system at any time -- didn't realize this at first
fileprivate func migrateToApplicationSupport() {
    let cacheDir = try! Folder.home.subfolder(at: "Library/Caches")
    let appSupDir = Folder.applicationSupport
    
    if (try? appSupDir.subfolder(named: "Downloads")) == nil,
        let downloadsDir = try? cacheDir.subfolder(at: "Downloads") {
        print("moved downloads folder")
        try! downloadsDir.move(to: appSupDir)
    }
    
    if (try? appSupDir.file(named: "favorites.json")) == nil,
        let oldFavorites = try? cacheDir.file(named: "favorites.json") {
        print("moved favorites")
        try! oldFavorites.move(to: appSupDir)
    }
    
    if (try? appSupDir.file(named: "downloads.json")) == nil,
        let oldDownloads = try? cacheDir.file(named: "downloads.json") {
        print("moved downloads file")
        try! oldDownloads.move(to: appSupDir)
    }
}

fileprivate var bandsSub: AnyCancellable?

fileprivate func migrateToV2(_ completion: @escaping () -> (), _ error: @escaping () -> ()) -> Bool {
    guard !UserDefaults.standard.bool(forKey: "v1.5-update") else { return true }
    let downloadsFile = FileSystemSourceStore(named: "downloads.json")
    let favoritesFile = FileSystemSourceStore(named: "favorites.json")
    
    print("\(downloadsFile.favs.count) downloads")
    print("\(favoritesFile.favs.count) favorites")
    
    let uniqueIds = Set(downloadsFile.favs.union(favoritesFile.favs).map(\.identifier))
    
    let api = APIClient()
    bandsSub = api.getMigrationItems(Array(uniqueIds))
        .receive(on: DispatchQueue.main)
        .sink { c in
            switch c {
            case .failure:
                error()
            default:
                break
            }
        } receiveValue: { items in
            performV2Migration(
                items,
                isDownloaded: { downloadsFile.favs.map(\.identifier).contains($0.recording.identifier) },
                isFavorite: { favoritesFile.favs.map(\.identifier).contains($0.recording.identifier) }
            )
            UserDefaults.standard.setValue(true, forKey: "v1.5-update")
            completion()
        }

    return false
}

fileprivate func performV2Migration(
    _ items: [MigrationItem],
    isDownloaded: (MigrationItem) -> Bool,
    isFavorite: (MigrationItem) -> Bool
) {
    let downloadsFolder = try! Folder.applicationSupport.subfolder(named: "Downloads")
        
    let stored: [StoredRecording] = items.map { item in
        if let downloadFolder = try? downloadsFolder.subfolder(named: item.recording.identifier),
           let metadata = try? downloadFolder.file(named: "metadata.json"),
           let data = try? metadata.read() {
            do {
                let songs = try JSONDecoder().decode([LegacySong].self, from: data)
                let newSongs = songs.map { song in
                    return Song(title: song.title, fileName: song.fileName, album: song.album ?? "\(item.band.name): \(item.performance.date)", track: song.track, length: song.length, recordingId: item.recording.id)
                }
                
                return StoredRecording(
                    band: Band(collection: item.band.collection, name: item.band.name, logoUrl: item.band.logoUrl),
                    performance: item.performance,
                    recording: item.recording,
                    songs: newSongs,
                    favorite: isFavorite(item),
                    downloadState: isDownloaded(item) ? .downloaded : .notDownloaded
                )
            } catch {
                if let error = error as? DecodingError {
                    print(error)
                }
            }
            return nil
        }
        
        return StoredRecording(
            band: Band(collection: item.band.collection, name: item.band.name, logoUrl: item.band.logoUrl),
            performance: item.performance,
            recording: item.recording,
            songs: [],
            favorite: isFavorite(item),
            downloadState: .notDownloaded
        )
    }.compactMap { $0 }
    
    let storage = AppStorageManager.shared
    stored.forEach(storage.addStoredRecording)
    storage.saveToDisk()
 }

//fileprivate extension StoredRecording {
//    init(legacySongs: [LegacySong]) {
//        let song = legacySongs.first!
//        
////        self.band = song.source.show.year.collection
//        
//    }
//}
