//
//  LibraryItemView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI
import Combine

struct LibraryItemView: View {
    @StateObject var viewModel: LibraryItemViewModel
    
    init(recordingId: String) {
        _viewModel = StateObject(wrappedValue: LibraryItemViewModel(app: app, recordingId: recordingId))
    }
    
    var body: some View {
        mainView
            .task {
                await viewModel.load()
            }
    }
    
    var mainView: some View {
        switch viewModel.loadedLibraryItem {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(
                LoadedLibraryItemView(
                    viewModel: viewModel,
                    loadedItem: t
                )
            ).id("library")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct LoadedLibraryItemView: View {
    @EnvironmentObject var audioSystem: AudioSystem
    @EnvironmentObject var favorites: Favorites
    @EnvironmentObject var downloads: Downloads
    
    @ObservedObject var viewModel: LibraryItemViewModel
    let loadedItem: LoadedLibraryItem
    
    var item: LibraryItem { loadedItem.item }
    
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
                ForEach(loadedItem.tracks, id: \.id) { track in
                    TrackRow(
                        index: track.track,
                        title: track.title,
                        length: track.length,
                        downloading: downloads.fileDownloadProgresses[track.fileName] != nil,
                        playing: audioSystem.playlist?.currentTrack.id == track.id
                    )
                        .onTapGesture {
                            let playlistTracks = loadedItem.tracks.map { track in
                                return Playlist.Track(
                                    id: track.id,
                                    title: track.title,
                                    fileName: track.fileName,
                                    length: track.length,
                                    album: "\(item.performance.date): \(loadedItem.item.performance.venue)",
                                    artist: loadedItem.item.band.name,
                                    audioUrl: URL(
                                        string: "https://archive.org/download/\(loadedItem.item.recording.identifier)/\(track.fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
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
            print(error)
        }
    }
    
    func onDownloadClick() {
        switch progress {
        case .notDownloaded:
            let batchDownload = BatchDownload(identifier: item.recording.identifier, fileNames: loadedItem.tracks.map(\.fileName))
            let downloader = downloads.addDownloader(recordingId: item.recording.id, identifier: item.recording.identifier)
            downloader.download(batchDownload: batchDownload)
        case .downloading:
            downloads.cancelDownloader(identifier: item.recording.identifier)
        case .downloaded:
            downloads.removeDownload(id: item.recording.id)
        }        
    }
}

//struct TrackWrapper: View {
//    let track: Track
//    @
//    
//    @EnvironmentObject var audioSystem: AudioSystem
//    private var cancellable: AnyCancellable?
//    
//    
//    var body: some View {
//        TrackRow(
//            index: track.track,
//            title: track.title,
//            length: track.length,
//            downloading: downloading,
//            playing: audioSystem.playlist?.currentTrack.id == track.id
//        )
//    }
//}
