//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/24/23.
//

import Foundation
import Combine

public class Favorites: ObservableObject {
    let p: Persistence
    
    var isFavorite: Bool = false
    
    public init(p: Persistence) {
        self.p = p
    }
    
    public func favorited(recordingId: String) -> Bool {
        return (try? p.recordingRepository.get(id: recordingId)?.favorite) ?? false
    }
    
    public func loadFavorites() throws -> [(Band, Performance, Recording)] {
        let recordings = try p.recordingRepository.getFavorites()
        print(recordings)
        
        let performanceIds = recordings.map(\.performanceId)
        let performanceMap: [String : Performance] = try p.performanceRepository.getIds(performanceIds).reduce(into: [:]) { partialResult, performance in
            partialResult[performance.id] = performance
        }
        
        let bandIds = performanceMap.values.map(\.bandId)
        let bandMap: [String : Band] = try p.bandRepository.getIds(bandIds).reduce(into: [:]) { partialResult, band in
            partialResult[band.id] = band
        }
        
        return recordings.compactMap { recording in
            print(recording)
            guard let performance = performanceMap[recording.performanceId] else { return nil }
            guard let band = bandMap[performance.bandId] else { return nil }
            
            return (band, performance, recording)
        }
    }
    
    public func loadFavorite(recordingId: String) throws -> (Band, Performance, Recording, [Track])? {
        let tracks = try p.trackRepository.getRecordingTracks(recordingId: recordingId)
        let recording = try p.recordingRepository.get(id: recordingId)
        guard let recording else { return nil }
        
        let performance = try p.performanceRepository.getIds([recording.performanceId]).first
        guard let performance else { return nil }
        
        let band = try p.bandRepository.getIds([performance.bandId]).first
        guard let band else { return nil }
        
        return (band, performance, recording, tracks)
    }
    
    public func persistFavorite(apiBand: APIBand, apiPerformance: APIPerformance, apiRecording: APIRecording, apiTracks: [APITrack]) throws {
        try p.sqliteConnection.transaction {
            try p.bandRepository.upsertApi(apiBand)
            try p.performanceRepository.upsertApi(apiPerformance)
            
            let recording = try (p.recordingRepository.get(id: apiRecording.id) ?? p.recordingRepository.insertApi(apiRecording))
            try p.recordingRepository.favorite(id: recording.id)
            
            for apiTrack in apiTracks {
                try p.trackRepository.upsertApi(apiTrack)
            }
        }
        
        objectWillChange.send()
    }
    
    public func unfavorite(recordingId: String) throws {
        try p.recordingRepository.unfavorite(id: recordingId)
        objectWillChange.send()
    }
}
