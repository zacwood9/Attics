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
func runMigrations() {
    migrateToApplicationSupport()
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

fileprivate func migrateToV2() {
    let downloadsFile = FileSystemSourceStore(named: "downloads.json")
}
