//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/24/23.
//

import Foundation

import Foundation
import SQLite

public struct Track: Codable {
    static var table: Table { TrackRepository.table }
    
    public let id: String
    public let title: String
    public let fileName: String
    public let track: Int
    public let length: String
    public let recordingId: String
    
    public static func decode(row: Row) -> Track? {
        do {
            let track: Track = try row.decode()
            return track
        } catch {
            logger.error("Failed to decode Track: \(error)")
            return nil
        }
    }
}

extension Track {
    public static func fromApi(api: APITrack) -> Self {
        self.init(id: api.id, title: api.title, fileName: api.fileName, track: api.track, length: api.length, recordingId: api.recordingId)
    }
}

public class TrackRepository {
    let db: Connection
    
    static let table = Table("tracks")
    var table: Table { TrackRepository.table }
    
    struct Rows {
        static let id = Expression<String>("id")
        static let title = Expression<String>("title")
        static let fileName = Expression<String>("fileName")
        static let track = Expression<Int>("track")
        static let length = Expression<String>("length")
        static let recordingId = Expression<String>("recordingId")
    }
    
    public init(db: Connection) {
        self.db = db
    }
    
    public func getRecordingTracks(recordingId: String) throws -> [Track] {
        return try getForRecordings(recordingIds: [recordingId])
    }
    
    public func getForRecordings(recordingIds: [String]) throws -> [Track] {
        let rows = try db.prepare(table.filter(recordingIds.contains(Rows.recordingId)))
        return rows.compactMap { Track.decode(row: $0) }
    }
    
    @discardableResult
    public func upsertApi(_ api: APITrack) throws -> Track {
        let track = Track.fromApi(api: api)
        try db.run(table.upsert(track, onConflictOf: Rows.id))
        return track
    }
    
    @discardableResult
    public func upsert(id: String, title: String, fileName: String, track: Int, length: String, recordingId: String) throws -> Track {
        let track = Track(id: id, title: title, fileName: fileName, track: track, length: length, recordingId: recordingId)
        try db.run(table.upsert(track, onConflictOf: Rows.id))
        return track
    }
}
