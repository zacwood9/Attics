import SwiftAudioEx
import Files
import SQLite

public class AtticsCore {
    public let apiClient: APIClient
    public let audioSystem: AudioSystem
    public let persistence: Persistence
    public let favorites: Favorites
    public let downloader: Downloader
    
    public var bandRepository: BandRepository { persistence.bandRepository }
    
    public init() {
        persistence = try! Persistence()
        apiClient = APIClient()
        audioSystem = AudioSystem()
        favorites = Favorites(p: persistence)
        downloader = Downloader(p: persistence)
        
        if let playlistData: PlaylistData = try? persistence.loadDecodable(at: .playlist) {
            audioSystem.loadPlaylistData(playlistData)
        }
    }
    
    public func beforeTermination() {
        if let playlist = audioSystem.playlist {
            do {
                try persistence.persistEncodable(playlist.playlistData, to: .playlist)
                print("persisted playlist")
            } catch {
                print(error)
            }
        }
    }
}
