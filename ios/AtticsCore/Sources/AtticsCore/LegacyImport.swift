//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/28/23.
//

import Foundation
import SQLite

enum LegacyModels {
    typealias Year = String
    
    struct Band: Codable {
        let collection: String
        let name: String
        let logoUrl: String
    }
    
    struct Show: Codable, Equatable, Hashable {
        let date: String
        let venue: String
        let city: String
        let state: String
        let numReviews: Int
        let numRecordings: Int
        let avgRating: Double
        let bandId: String
    }

    struct Source: Codable, Equatable, Hashable {
        let id: String
        let identifier: String
        let transferer: String
        let source: String
        let avgRating: Double
        let atticsDownloads: Int
        let archiveDownloads: Int
        let numReviews: Int
        let lineage: String
        let performanceId: String
        
        var downloads: Int {
            atticsDownloads + archiveDownloads
        }
    }
    
    struct Song: Equatable, Hashable, Codable, Identifiable {
        var id: String {
            fileName
        }
        
        let title: String
        let fileName: String
        let album: String
        let track: Int
        let length: String
        let recordingId: String
    }
    
    enum DownloadState : Codable, Equatable {
        case notDownloaded
        case downloaded
        case downloading([Song : Double])
        
        init(from decoder: Decoder) throws {
            let v = try decoder.singleValueContainer()
            if let d = try? v.decode(String.self) {
                switch d {
                case "downloaded": self = .downloaded
                case "not_downloaded": self = .notDownloaded
                default: throw DecodingError.dataCorruptedError(in: v, debugDescription: "unable to decode download state")
                }
            } else if let d = try? v.decode([Song : Double].self) {
                self = .downloading(d)
            } else {
                throw DecodingError.dataCorruptedError(in: v, debugDescription: "unable to decode download state")
            }
        }
        
        func encode(to encoder: Encoder) throws {
            var container = encoder.singleValueContainer()
            switch self {
            case .downloaded: try container.encode("downloaded")
            case .notDownloaded: try container.encode("not_downloaded")
            case .downloading(let d): try container.encode(d)
            }
        }
    }


    struct StoredRecording : Codable, Equatable, Hashable {
        static func == (lhs: StoredRecording, rhs: StoredRecording) -> Bool {
            lhs.id == rhs.id
        }
        
        var id: String {
            recording.id
        }
        
        var band: Band
        var performance: Show
        var recording: Source
        var songs: [Song]
        var favorite: Bool
        var downloadState: DownloadState
        
        var hashValue: Int { id.hashValue }
        func hash(into hasher: inout Hasher) {
            id.hash(into: &hasher)
        }
    }
}

struct PendingImport : Codable {
    static let table = Table("pending_imports")
    enum Columns {
        static let id = Expression<String>("id")
        static let pending = Expression<Bool>("pending")
        static let recordingId = Expression<String>("recordingId")
    }
    
    let id: String
    let pending: Bool
    let recordingId: String
    
    var table: Table { PendingImport.table }
    
    static func all(_ p: Persistence) throws -> [PendingImport] {
        let rows = try p.db.prepare(table)
        return rows.compactMap { PendingImport.decode(row: $0) }
    }
    
    static func allPending(_ p: Persistence) throws -> [PendingImport] {
        let rows = try p.db.prepare(table.where(Columns.pending))
        return rows.compactMap { PendingImport.decode(row: $0) }
    }
    
    @discardableResult
    static func create(_ p: Persistence, recordingId: String) throws -> PendingImport {
        let pendingImport = PendingImport(id: UUID().uuidString, pending: true, recordingId: recordingId)
        try p.db.run(table.insert(pendingImport))
        logger.info("Created pending import for recording \(recordingId)")
        return pendingImport
    }
    
    func markComplete(_ p: Persistence) throws {
        try p.db.run(
            PendingImport.table.filter(Columns.id == id).update(Columns.pending <- false)
        )
        logger.info("Marked pending import for recording \(recordingId) as complete")
    }
    
    private static func decode(row: Row) -> PendingImport? {
        do {
            let i: PendingImport = try row.decode()
            return i
        } catch {
            logger.error("Failed to decode PendingImport: \(error)")
            return nil
        }
    }
}

public enum ImportState {
    case waiting
    case importing
    case imported
    case failed
}

public class LegacyImport: ObservableObject {
    let p: Persistence
    let apiClient: APIClient
    
    init(p: Persistence, apiClient: APIClient) {
        self.p = p
        self.apiClient = apiClient
    }
    
    public func importNeeded() -> Bool {
        let r = legacyStoredRecordings()
        if r.isEmpty {
            return false
        } else {
            logger.debug("\(r.map(\.recording.identifier))")
            return true
        }
    }
    
   public func run() async throws {
       let storedRecordings = legacyStoredRecordings()
       for storedRecording in storedRecordings {
           try await upsert(storedRecording)
       }
           
       logger.info("Completed legacy import")
   }
    
    private func upsert(_ stored: LegacyModels.StoredRecording) async throws {
        guard !stored.songs.isEmpty else { return }
        let recordingPage = try await apiClient.getRecording(recordingId: stored.recording.id)
        
        try p.db.transaction {
            try p.bandRepository.upsert(id: stored.performance.bandId, collection: stored.band.collection, name: stored.band.name)
            try p.performanceRepository.upsert(id: stored.recording.performanceId, date: stored.performance.date, venue: stored.performance.venue, city: stored.performance.city, state: stored.performance.state, bandId: stored.performance.bandId)
            try p.recordingRepository.upsert(id: stored.recording.id, identifier: stored.recording.identifier, transferer: stored.recording.transferer, source: stored.recording.source, lineage: stored.recording.lineage, favorite: stored.favorite, downloaded: stored.downloadState == .downloaded, performanceId: stored.recording.performanceId)
            
            for song in stored.songs {
                if let trackId = recordingPage.tracks.first(where: { $0.fileName == song.fileName })?.id {
                    try p.trackRepository.upsert(id: trackId, title: song.title, fileName: song.fileName, track: song.track, length: song.length, recordingId: song.recordingId)
                }
            }
            
            removeStoredRecording(stored)
        }
        
        logger.info("Finished importing legacy data for \(stored.recording.identifier).")
    }
    
    private func legacyStoredRecordings() -> [LegacyModels.StoredRecording] {
        do {
            return try p.loadDecodable(at: .legacyStorage)
        } catch {
            logger.debug("Failed to load legacy recordings: \(error)")
            return []
        }
    }
    
    private func removeStoredRecording(_ recording: LegacyModels.StoredRecording) {
        let recordings = legacyStoredRecordings()
        do {
            try p.persistEncodable(recordings.filter { $0 != recording }, to: .legacyStorage)
        } catch {
            logger.error("Failed to remove LegacyModels.StoredRecording(id: \(recording.id): \(error)")
        }
    }
}
