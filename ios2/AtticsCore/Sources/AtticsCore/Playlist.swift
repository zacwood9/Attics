import Foundation

public class Playlist: ObservableObject {
    public enum Types: Codable {
        case recording
        case created
    }
    public struct Track: Codable {
        public let id: String
        public let title: String
        public let fileName: String
        public let length: String
        public let album: String
        public let artist: String
        public let audioUrl: URL
    }
        
    public var playlistId: String
    public var playlistType: Types = .recording
    
    @Published public internal(set) var tracks: [Track]
    @Published public internal(set) var currentTrack: Track
    @Published public internal(set) var secondsPlayed: Double = 0
    
    public var nextTrack: Track? {
        guard let currentTrackIndex, currentTrackIndex + 1 < tracks.count else { return nil }
        return tracks[currentTrackIndex + 1]
    }
    
    var playlistData: PlaylistData {
        PlaylistData(
            id: playlistId,
            type: playlistType,
            tracks: tracks,
            currentTrack: currentTrack,
            secondsPlayed: secondsPlayed
        )
    }
    
    internal var currentTrackIndex: Int? {
        return tracks.firstIndex { $0.id == currentTrack.id }
    }
    
    public init(playlistId: String, tracks: [Track], currentTrack: Track, secondsPlayed: Double = 0) {
        self.playlistId = playlistId
        self.tracks = tracks
        self.currentTrack = currentTrack
        self.secondsPlayed = secondsPlayed
    }
}

struct PlaylistData: Codable {
    let id: String
    let type: Playlist.Types
    let tracks: [Playlist.Track]
    let currentTrack: Playlist.Track
    let secondsPlayed: Double
}
