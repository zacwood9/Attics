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

public enum ImportState {
    case importing
    case imported
    case failed
}

public class LegacyImport {
    @Published var state: ImportState = .importing
    
    let p: Persistence
    let apiClient: APIClient
    
    init(p: Persistence, apiClient: APIClient) {
        self.p = p
        self.apiClient = apiClient
    }
    
    func run() async throws {
        state = .importing
        
        do {
            let storedRecordings = try legacyStoredRecordings()
            for storedRecording in storedRecordings {
                try await upsert(storedRecording)
            }
            state = .imported
        } catch {
            state = .failed
            throw error
        }
    }
    
    private func upsert(_ stored: LegacyModels.StoredRecording) async throws {
        guard !stored.songs.isEmpty else { return }
        print("upserting")
        
        let recordingPage = try await apiClient.getRecordingAsync(recordingId: stored.recording.id)
        
        try p.db.transaction {
            try p.bandRepository.upsert(id: stored.performance.bandId, collection: stored.band.collection, name: stored.band.name)
            try p.performanceRepository.upsert(id: stored.recording.performanceId, date: stored.performance.date, venue: stored.performance.venue, city: stored.performance.city, state: stored.performance.state, bandId: stored.performance.bandId)
            try p.recordingRepository.upsert(id: stored.recording.id, identifier: stored.recording.identifier, transferer: stored.recording.transferer, source: stored.recording.source, lineage: stored.recording.lineage, favorite: stored.favorite, downloaded: stored.downloadState == .downloaded, performanceId: stored.recording.performanceId)
            
            for song in stored.songs {
                if let trackId = recordingPage.tracks.first(where: { $0.fileName == song.fileName })?.id {
                    try p.trackRepository.upsert(id: trackId, title: song.title, fileName: song.fileName, track: song.track, length: song.length, recordingId: song.recordingId)
                }
                
            }
        }
    }
    
    private func legacyStoredRecordings() throws -> [LegacyModels.StoredRecording] {
        return try p.loadDecodable(at: .legacyStorage)
    }
}
