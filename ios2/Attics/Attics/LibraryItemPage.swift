//
//  LibraryItemView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI
import Combine

struct LibraryItemPage: View {
    let recordingId: String
    
    @State var result: APIResult<Library.Item> = .loading
    
    var body: some View {
        ResultView(result) { result in
            LibraryItemView(item: result)
        }
        .task { await load() }
        .refreshable { Task { await load() } }
        .onReceive(app.downloads.$activeDownloadIdentifiers, perform: { _ in
            Task { await load() }
        })
        .onReceive(app.downloads.$downloadedRecordingIds, perform: { _ in
            Task { await load() }
        })
        .onReceive(app.favorites.objectWillChange, perform: { _ in
            Task { await load() }
        })
    }
    
    private func load() async {
        do {
            if let item = try app.library.loadItem(recordingId: recordingId) {
                result = .success(item)
            }
        } catch {
            logger.error("Failed to load LibraryItem: \(error)")
            result = .error(error)
        }
    }
}

struct LibraryItemView: View {
    let item: Library.Item
    
    @StateObject var audioSystem = app.audioSystem
    @StateObject var favorites = app.favorites
    @StateObject var downloads = app.downloads
    
    var progress: DownloadProgress {
        if downloads.activeDownloadIdentifiers.contains(item.recording.identifier) {
            return .downloading
        } else if downloads.downloadedRecordingIds.contains(item.recording.id) {
            return .downloaded
        } else {
            return .notDownloaded
        }
    }
    
    var body: some View {
        List {
            Section {
                PlaylistHeader(bandName: item.band.name, venue: item.performance.venue, date: item.performance.date, isFavorite: favorites.favorited(recordingId: item.recording.id), downloadProgress: progress, onFavoriteClick: onFavoriteClick, onDownloadClick: onDownloadClick)
            }
            
            Section {
                ForEach(item.tracks, id: \.id) { track in
                    TrackRow(
                        index: track.track,
                        title: track.title,
                        length: track.length,
                        downloading: downloads.fileDownloadProgresses[track.fileName] != nil,
                        playing: audioSystem.playlist?.currentTrack.id == track.id
                    )
                        .onTapGesture {
                            let playlistTracks = item.tracks.map { track in
                                return Playlist.Track(
                                    id: track.id,
                                    title: track.title,
                                    fileName: track.fileName,
                                    length: track.length,
                                    album: "\(item.performance.date): \(item.performance.venue)",
                                    artist: item.band.name,
                                    audioUrl: URL(
                                        string: "https://archive.org/download/\(item.recording.identifier)/\(track.fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
                                    )!
                                )
                            }
                            
                            audioSystem.loadPlaylist(initialId: track.id, bandName: item.band.name, recordingId: item.recording.id, tracks: playlistTracks)
                            audioSystem.play(id: track.id)
                        }
                }
            }
        }
    }
    
    func onFavoriteClick() {
        do {
            if favorites.favorited(recordingId: item.recording.id) {
                try favorites.unfavorite(recordingId: item.recording.id)
            } else {
                try favorites.favorite(recordingId: item.recording.id)
            }
        } catch {
            logger.error("Failed to favorite LibraryItem(recordingId: \(item.recording.id)): \(error)")
        }
    }
    
    func onDownloadClick() {
        switch progress {
        case .notDownloaded:
            let batchDownload = BatchDownload(identifier: item.recording.identifier, fileNames: item.tracks.map(\.fileName))
            let downloader = downloads.addDownloader(recordingId: item.recording.id, identifier: item.recording.identifier)
            downloader.download(batchDownload: batchDownload)
        case .downloading:
            downloads.cancelDownloader(identifier: item.recording.identifier)
        case .downloaded:
            downloads.removeDownload(id: item.recording.id)
        }        
    }
}
