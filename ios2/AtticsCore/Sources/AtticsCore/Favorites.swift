//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/24/23.
//

import Foundation
import Combine
import SQLite

public class Favorites: ObservableObject {
    let p: Persistence
    let library: Library
    
    @Published public private(set) var favoritedRecordingIds: Set<String>
    
    public init(p: Persistence, library: Library) throws {
        self.p = p
        self.library = library
        
        favoritedRecordingIds = try Set(p.recordingRepository.getFavorites().filter(\.favorite).map(\.id))
    }
    
    public func reloadFavoritedRecordingIds() {
        do {
            favoritedRecordingIds = try Set(p.recordingRepository.getFavorites().filter(\.favorite).map(\.id))
        } catch {
            logger.error("Failed to reload favorited recording ids: \(error)")            
        }
    }
    
    public func favorited(recordingId: String) -> Bool {
        return favoritedRecordingIds.contains(recordingId)
    }
    
    public func persistFavorite(apiBand: APIBand, apiPerformance: APIPerformance, apiRecording: APIRecording, apiTracks: [APITrack]) throws {
        try library.persistApiItem(apiBand: apiBand, apiPerformance: apiPerformance, apiRecording: apiRecording, apiTracks: apiTracks)
        try favorite(recordingId: apiRecording.id)
    }
    
    public func favorite(recordingId: String) throws {
        try p.db.run(
            p.recordingRepository.table
                .where(RecordingRepository.Rows.id == recordingId)
                .update(RecordingRepository.Rows.favorite <- true)
        )
        favoritedRecordingIds.insert(recordingId)
    }
    
    public func unfavorite(recordingId: String) throws {
        try p.recordingRepository.unfavorite(id: recordingId)
        favoritedRecordingIds.remove(recordingId)
    }
}
