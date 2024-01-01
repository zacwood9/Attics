//
//  Library.swift
//
//
//  Created by Zachary Wood on 12/28/23.
//

import Foundation
import Combine


public class Library {
    public struct Item: Codable {
        public let band: Band
        public let performance: Performance
        public let recording: Recording
        public let tracks: [Track]
    }
    
    let p: Persistence
    
    init(p: Persistence) {
        self.p = p
    }
    
    public func persistApiItem(apiBand: APIBand, apiPerformance: APIPerformance, apiRecording: APIRecording, apiTracks: [APITrack]) throws {
        try p.db.transaction {
            try p.bandRepository.upsertApi(apiBand)
            try p.performanceRepository.upsertApi(apiPerformance)
            
            let _ = try (p.recordingRepository.get(id: apiRecording.id) ?? p.recordingRepository.insertApi(apiRecording))
            
            for apiTrack in apiTracks {
                try p.trackRepository.upsertApi(apiTrack)
            }
        }
        
    }
    
    public func loadItems() async throws -> [Library.Item] {
        let recordings = try p.recordingRepository.getPersisted()
        let recordingIds = recordings.map(\.id)
        
        let tracks = try p.trackRepository.getForRecordings(recordingIds: recordingIds)
        let tracksGroupedByRecording: [String : [Track]] = tracks.reduce(into: [:]) { partialResult, track in
            if var tracks = partialResult[track.recordingId] {
                tracks.append(track)
                partialResult[track.recordingId] = tracks
            } else {
                partialResult[track.recordingId] = [track]
            }
        }
        
        let performanceIds = recordings.map(\.performanceId)
        let performanceMap: [String : Performance] = try p.performanceRepository.getIds(performanceIds).reduce(into: [:]) { partialResult, performance in
            partialResult[performance.id] = performance
        }
        
        let bandIds = performanceMap.values.map(\.bandId)
        let bandMap: [String : Band] = try p.bandRepository.getIds(bandIds).reduce(into: [:]) { partialResult, band in
            partialResult[band.id] = band
        }
        
        return recordings.compactMap { recording in
            guard let performance = performanceMap[recording.performanceId] else { return nil }
            guard let band = bandMap[performance.bandId] else { return nil }
            guard let tracks = tracksGroupedByRecording[recording.id] else { return nil }
            
            return Library.Item(band: band, performance: performance, recording: recording, tracks: tracks)
        }
    }
    
    public func loadItem(recordingId: String) throws -> Library.Item? {        
        let recording = try p.recordingRepository.get(id: recordingId)
        guard let recording else { return nil }
        
        let tracks = try p.trackRepository.getRecordingTracks(recordingId: recordingId)
        
        let performance = try p.performanceRepository.getIds([recording.performanceId]).first
        guard let performance else { return nil }
        
        let band = try p.bandRepository.getIds([performance.bandId]).first
        guard let band else { return nil }
        
        return Library.Item(band: band, performance: performance, recording: recording, tracks: tracks)
    }
}
