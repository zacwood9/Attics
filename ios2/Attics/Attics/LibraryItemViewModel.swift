//
//  LibraryItemViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import Foundation
import Combine

struct LoadedLibraryItem: Decodable {
    let item: LibraryItem
    let tracks: [Track]
}

class LibraryItemViewModel: ObservableObject {
    let app: AtticsCore
    let recordingId: String
    
    @Published var loadedLibraryItem: APIResult<LoadedLibraryItem> = .loading
    
    init(app: AtticsCore, recordingId: String) {
        self.app = app
        self.recordingId = recordingId
    }
    
    @MainActor
    func load() async {
        do {
            if let (band, performance, recording, tracks) = try app.favorites.loadFavorite(recordingId: recordingId) {
                loadedLibraryItem = .success(
                    LoadedLibraryItem(
                        item: LibraryItem(band: band, performance: performance, recording: recording),
                        tracks: tracks
                    )
                )
            } else {
                print("failed to load favorite")
            }
        } catch {
            loadedLibraryItem = .error(error)
        }
    }
    
    func isTrackPlaying(id: String) -> Bool {
        guard let playlist = app.audioSystem.playlist else { return false }
        return playlist.currentTrack.id == id
    }
}
