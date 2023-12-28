//
//  RecordingView.swift
//  Attics
//
//  Created by Zachary Wood on 12/21/23.
//

import SwiftUI
import AtticsCore

struct RecordingView: View {
    @StateObject var recordingViewModel: RecordingViewModel
    
    init(recordingId: String) {
        self._recordingViewModel = StateObject(
            wrappedValue: RecordingViewModel(
                app: app,
                recordingId: recordingId
            )
        )
    }
    
    var body: some View {
        mainView
            .navigationBarTitleDisplayMode(.inline)
            .onAppear { recordingViewModel.load() }
            .environmentObject(recordingViewModel)
    }
    
    var mainView: some View {
        switch recordingViewModel.recordingView {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(RecordingLoadedView(recordingView: t)).id("recording")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct RecordingLoadedView: View {
    var recordingView: APIRecordingPage
    @EnvironmentObject var viewModel: RecordingViewModel
    @EnvironmentObject var audioSystem: AudioSystem
    @EnvironmentObject var favorites: Favorites
    @EnvironmentObject var downloads: Downloads
    
    var progress: DownloadProgress {
        if downloads.activeDownloadIdentifiers.contains(recordingView.recording.identifier) {
            return .downloading
        } else if downloads.downloadedRecordingIds.contains(recordingView.recording.id) {
            return .downloaded
        } else {
            return .notDownloaded
        }
    }
    
    var body: some View {
        let isFavorite = favorites.favorited(recordingId: recordingView.recording.id)
        List {
            Section {
                PlaylistHeader(bandName: recordingView.band.name, venue: recordingView.performance.venue, date: recordingView.performance.date, isFavorite: isFavorite, downloadProgress: progress, onFavoriteClick: onFavoriteClick, onDownloadClick: onDownloadClick)
            }
            
            Section {
                ForEach(recordingView.tracks.indices, id: \.self) { i in
                    let track = recordingView.tracks[i]
                    let playing = audioSystem.playlist?.currentTrack.id == track.id
                    TrackRow(
                        index: i + 1,
                        title: track.title,
                        length: track.length,
                        downloading: downloads.fileDownloadProgresses[track.fileName] != nil,
                        playing: playing
                    )
                    .onTapGesture {                        
                        let playlistTracks = recordingView.tracks.map { track in
                            return Playlist.Track(
                                id: track.id,
                                title: track.title,
                                fileName: track.fileName,
                                length: track.length,
                                album: track.album,
                                artist: recordingView.band.name,
                                audioUrl: URL(string: track.downloadUrl)!
                            )
                        }
                        
                        audioSystem.loadPlaylist(
                            initialId: track.id,
                            bandName: recordingView.band.name,
                            recordingId: recordingView.recording.id,
                            tracks: playlistTracks
                        )
                        
                        audioSystem.play(id: track.id)
                        
                    }
                }
            }
            
        }
        
    }
    
    func onFavoriteClick() {
        do {
            if favorites.favorited(recordingId: recordingView.recording.id) {
                try favorites.unfavorite(recordingId: recordingView.recording.id)
            } else {
                try favorites.persistFavorite(apiBand: recordingView.band, apiPerformance: recordingView.performance, apiRecording:  recordingView.recording, apiTracks: recordingView.tracks)
            }
        } catch {
            print(error)
        }
    }
    
    func onDownloadClick() {
        switch progress {
        case .notDownloaded:
            let batchDownload = BatchDownload(identifier: recordingView.recording.identifier, fileNames: recordingView.tracks.map(\.fileName))
            let downloader = downloads.addDownloader(recordingId: recordingView.recording.id, identifier: recordingView.recording.identifier)
            downloader.download(batchDownload: batchDownload)
        case .downloading:
            downloads.cancelDownloader(identifier: recordingView.recording.identifier)
        case .downloaded:
            downloads.removeDownload(id: recordingView.recording.id)
        }
    }
}
