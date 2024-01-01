//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/30/23.
//

import Foundation
import SQLite

public class History: ObservableObject {
    public struct Item: Codable, Identifiable {
        public let id: String
        public let trackTitle: String
        public let bandName: String
        public let date: String
        public let location: String
        public let playedAt: Date
        public let recordingId: String
    }
    
    let p: Persistence
    
    public init(p: Persistence) {
        self.p = p
    }
    
    public func clear() throws {
        try p.db.run(TrackPlay.table.delete())
        self.objectWillChange.send()
    }
    
    public func recordEntry(for trackId: String) {
        do {
            try TrackPlay.create(p, trackId: trackId)
            
            Task {
                await MainActor.run { self.objectWillChange.send() }
            }
        } catch {
            logger.error("Failed to record history entry for Track(id: \(trackId)): \(error)")
        }
    }
    
    public func getItems() throws -> [History.Item] {
        let trackPlays = try TrackPlay.all(p)
        let trackIds = trackPlays.map(\.trackId)
        let tracks = try p.db.prepare(
            TrackRepository.table.where(trackIds.contains(TrackRepository.Rows.id))
        ).compactMap { Track.decode(row: $0) }
        
        var recordingIds: Set<String> = []
        tracks.forEach { recordingIds.insert($0.recordingId) }
        
        let joinedTable = TrackPlay.table.join(
            Track.table, 
            on: Track.table[TrackRepository.Rows.id] == TrackPlay.Columns.trackId
        ).join(
            RecordingRepository.table, 
            on: TrackRepository.Rows.recordingId == RecordingRepository.table[RecordingRepository.Rows.id]
        ).join(
            PerformanceRepository.table, 
            on: RecordingRepository.Rows.performanceId == PerformanceRepository.table[PerformanceRepository.Rows.id]
        ).join(
            BandRepository.table, 
            on: PerformanceRepository.Rows.bandId == BandRepository.table[BandRepository.Rows.id]
        ).order(
            TrackPlay.table[TrackPlay.Columns.playedAt].desc
        )
        
        return try p.db.prepare(joinedTable).map { decode($0) }
    }
    
    private func decode(_ row: Row) -> History.Item {
        return History.Item(
            id: row[TrackPlay.table[TrackPlay.Columns.id]],
            trackTitle: row[TrackRepository.table[TrackRepository.Rows.title]],
            bandName: row[BandRepository.table[BandRepository.Rows.name]],
            date: row[PerformanceRepository.table[PerformanceRepository.Rows.date]],
            location: row[PerformanceRepository.table[PerformanceRepository.Rows.venue]],
            playedAt: row[TrackPlay.table[TrackPlay.Columns.playedAt]],
            recordingId: row[Track.table[TrackRepository.Rows.recordingId]]
        )
    }
}
