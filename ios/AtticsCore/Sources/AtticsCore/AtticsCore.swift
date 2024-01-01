import Foundation
import SwiftAudioEx
import Files
import SQLite
import os

public let logger = Logger()

public class AtticsCore {    
    public let apiClient: APIClient
    public let audioSystem: AudioSystem
    public let persistence: Persistence
    public let favorites: Favorites
    public let downloads: Downloads
    public let library: Library
    public let legacyImport: LegacyImport
    public let history: History
    
    public var bandRepository: BandRepository { persistence.bandRepository }
    
    public init() {
        persistence = try! Persistence()
        apiClient = APIClient()
        audioSystem = AudioSystem()
        
        downloads = try! Downloads(p: persistence)
        library = Library(p: persistence)
        favorites = try! Favorites(p: persistence, library: library)
        legacyImport = LegacyImport(p: persistence, apiClient: apiClient)
        history = History(p: persistence)
        
        if let playlistData: PlaylistData = try? persistence.loadDecodable(at: .playlist) {
            audioSystem.loadPlaylistData(playlistData)
        }
    }

    public func beforeTermination() {
        if let playlist = audioSystem.playlist {
            do {
                try persistence.persistEncodable(playlist.playlistData, to: .playlist)
            } catch {
                logger.error("Failed to persist playlist: \(error)")
            }
        }
    }
}
