import SwiftAudioEx
import Files
import SQLite

public class AtticsCore {
    public let apiClient: APIClient
    public let audioSystem: AudioSystem
    public let persistence: Persistence
    public let favorites: Favorites
    public let downloads: Downloads
    let legacyImport: LegacyImport
    
    public var bandRepository: BandRepository { persistence.bandRepository }
    
    public init() {
        persistence = try! Persistence()
        apiClient = APIClient()
        audioSystem = AudioSystem()
        favorites = Favorites(p: persistence)
        downloads = try! Downloads(p: persistence)
        legacyImport = LegacyImport(p: persistence, apiClient: apiClient)
        
        if let playlistData: PlaylistData = try? persistence.loadDecodable(at: .playlist) {
            audioSystem.loadPlaylistData(playlistData)
        }
    }
    
    public func runImport() {
        Task {
            do {
                try await legacyImport.run()
                await MainActor.run {
                    downloads.reloadDownloadedRecordings()
                }
            } catch {
                print("failed to import: \(error)")
            }
        }
    }
    
    public func beforeTermination() {
        if let playlist = audioSystem.playlist {
            do {
                try persistence.persistEncodable(playlist.playlistData, to: .playlist)
            } catch {
                print(error)
            }
        }
    }
}
