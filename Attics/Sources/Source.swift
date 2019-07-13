//
//  Source.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import CoreData

struct Source: Codable, Equatable {
    let id: Int
    let identifier: String
    let transferer: String
    let source: String
    let avgRating: Double
    let downloads: Int
    let numReviews: Int
//    let description: String
    let lineage: String
    let isFavorite: Bool
    let show: Show
}

extension Source {
    init(from mo: SourceMO) {
        self.id = Int(mo.id)
        self.identifier = mo.identifier ?? ""
        self.transferer = mo.transferer ?? ""
        self.source = mo.source ?? ""
        self.avgRating = mo.avgRating
        self.downloads = Int(mo.downloads)
        self.numReviews = Int(mo.numReviews)
        self.lineage = mo.lineage ?? ""
        self.isFavorite = mo.isFavorite
        if let showMO = mo.show {
            self.show = Show(showMO)
        } else {
            fatalError("Source.init(): Source managed object does not belong to a show")
        }
    }
}

extension SourceMO {
    convenience init(_ source: Source, into context: NSManagedObjectContext) {
        let sourceED = NSEntityDescription.entity(forEntityName: "Source", in: context)!
        self.init(entity: sourceED, insertInto: context)
        self.id = Int64(source.id)
        self.identifier = source.identifier
        self.transferer = source.transferer
        self.source = source.source
        self.avgRating = source.avgRating
        self.downloads = Int64(source.downloads)
        self.numReviews = Int64(source.numReviews)
        self.lineage = source.lineage
        self.show = ShowMO.findOrCreate(from: source.show, in: context)
        try! context.save()
    }
    
    static func findOrCreate(from source: Source, in context: NSManagedObjectContext) -> SourceMO {
        let fr = fetchRequest() as NSFetchRequest<SourceMO>
        fr.predicate = NSPredicate(format: "identifier = %@", source.identifier)
        do {
            let result = try context.fetch(fr)
            if let source = result.first {
                return source
            }
        } catch {
            print("SourceMO.findOrCreate() fetch request failed: \(error)")
        }
        return SourceMO.init(source, into: context)
    }
}


extension Source {
    enum RecordingType: String {
        case sbd = "SBD", aud = "AUD", matrix = "MATRIX", unknown = ""
    }
    
    var type: RecordingType {
        let s = source.lowercased()
        for type in ["mtx", "matrix"] {
            if s.contains(type) { return .matrix }
        }
        for type in ["aud", "audience"] {
            if s.contains(type) { return .aud }
        }
        for type in ["sbd", "soundboard"] {
            if s.contains(type) { return .sbd }
        }
        return .unknown
    }
}
