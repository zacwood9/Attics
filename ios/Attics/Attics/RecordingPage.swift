//
//  RecordingView.swift
//  Attics
//
//  Created by Zachary Wood on 12/21/23.
//

import SwiftUI
import AtticsCore

struct RecordingPage: View {
    let recordingIdOrIdentifier: String
    
    @State var result: APIResult<APIRecordingPage> = .loading
    @State var showNetworkAlert = false
    
    @StateObject var downloads = app.downloads
    @StateObject var favorites = app.favorites
    @StateObject var audioSystem = app.audioSystem
    @EnvironmentObject var networkMonitor: NetworkMonitor
    
    var body: some View {
        ResultView(result) { recordingPage in
            RecordingView(
                item: recordingPage,
                favorite: favorites.favorited(recordingId: recordingPage.recording.id),
                progress: progress(item: recordingPage),
                downloadItems: downloads.fileDownloadProgresses,
                isPlayable: isPlayable(recordingPage),
                onTrackClick: onTrackClick,
                onFavoriteClick: onFavoriteClick,
                onDownloadClick: onDownloadClick
            )
        }
            .atticsNavigationBar()
            .navigationBarTitleDisplayMode(.inline)
            .task {
                switch result {
                case .success:
                    break
                case .error:
                    result = .loading
                    await load()
                case .loading:
                    await load()
                }
            }
            .toolbar {
                ShareLink(item: "https://attics.io/recordings/\(recordingIdOrIdentifier)")
            }
            .refreshable { Task { await load() } }
            .alert(isPresented: $showNetworkAlert) {
                Alert(
                    title: Text("Unable to play track"),
                    message: Text("Network connection appears to be offline.")
                )
            }
    }
    
    private func progress(item: APIRecordingPage) -> DownloadProgress {
        if app.downloads.activeDownloadIdentifiers.contains(item.recording.identifier) {
            let items = item.tracks.compactMap { app.downloads.fileDownloadProgresses[$0.fileName] }
            let totalWritten = items.reduce(0, { sum, item in sum + item.bytesWritten })
            let totalExpected = items.reduce(0, { sum, item in sum + (item.bytesExpected ?? 0) })
            let totalEstimatedSeconds = items.reduce(0, { sum, item in sum + (item.estimatedTimeRemaining ?? 0) })
            
            return .downloading(totalWritten, totalExpected, totalEstimatedSeconds)
        } else if app.downloads.downloadedRecordingIds.contains(item.recording.id) {
            return .downloaded
        } else {
            return .notDownloaded
        }
    }
    
    private func onFavoriteClick(item: APIRecordingPage) {
        do {
            if favorites.favorited(recordingId: item.recording.id) {
                try favorites.unfavorite(recordingId: item.recording.id)
            } else {
                try favorites.persistFavorite(apiBand: item.band, apiPerformance: item.performance, apiRecording: item.recording, apiTracks: item.tracks)
            }
            
            UIDevice.vibrate(style: .medium)
        } catch {
            logger.error("Failed to favorite LibraryItem(recordingId: \(item.recording.id)): \(error)")
        }
    }
    
    private func onDownloadClick(item: APIRecordingPage) {
        switch progress(item: item) {
        case .notDownloaded:
            do {
                try app.library.persistApiItem(apiBand: item.band, apiPerformance: item.performance, apiRecording: item.recording, apiTracks: item.tracks)
                
                let batchDownload = BatchDownload(identifier: item.recording.identifier, fileNames: item.tracks.map(\.fileName))
                let downloader = downloads.addDownloader(recordingId: item.recording.id, identifier: item.recording.identifier)
                downloader.download(batchDownload: batchDownload)
            } catch {
                logger.error("Failed to start download on RecordingPage(identifier: \(item.recording.identifier)): \(error)")
                return
            }
        case .downloading:
            downloads.cancelDownloader(identifier: item.recording.identifier)
        case .downloaded:
            downloads.removeDownload(id: item.recording.id)
        }
        
        UIDevice.vibrate(style: .medium)
    }
    
    private func load() async {
        do {
            let page = try await app.apiClient.getRecording(recordingId: recordingIdOrIdentifier)
            result = .success(page)
        } catch {
            logger.error("Failed to fetch RecordingPage: \(error)")
            result = .error(error)
        }
    }
    
    private func isPlayable(_ item: APIRecordingPage) -> Bool {
        if networkMonitor.status != .disconnected {
            return true
        }
        
        return app.downloads.downloadedRecordingIds.contains(item.recording.id)
    }
    
    private func onTrackClick(_ item: APIRecordingPage, _ track: APITrack) {
        guard isPlayable(item) else {
            showNetworkAlert = true
            return
        }
        
        do {
            try app.library.persistApiItem(apiBand: item.band, apiPerformance: item.performance, apiRecording: item.recording, apiTracks: item.tracks)
            
            let playlistTracks = item.tracks.map { track in
                return Playlist.Track(
                    id: track.id,
                    title: track.title,
                    fileName: track.fileName,
                    length: track.length,
                    album: "\(item.performance.date): \(item.performance.venue)",
                    artist: item.band.name,
                    audioUrl: app.persistence.trackUrl(recordingIdentifier: item.recording.identifier, fileName: track.fileName)
                )
            }
            
            audioSystem.loadPlaylist(initialId: track.id, bandName: item.band.name, recordingId: item.recording.id, tracks: playlistTracks)
            audioSystem.play(id: track.id)
            
            UIDevice.vibrate(style: .select)
        } catch {
            logger.error("Failed to play \(track.fileName): \(error)")
        }
    }
}

struct RecordingView: View {
    let item: APIRecordingPage
    let favorite: Bool
    let progress: DownloadProgress
    let downloadItems: [String : DownloadItem]
    let isPlayable: Bool
    let onTrackClick: (APIRecordingPage, APITrack) -> Void
    let onFavoriteClick: (APIRecordingPage) -> Void
    let onDownloadClick: (APIRecordingPage) -> Void
    
    var body: some View {
        List {
            Section {
                PlaylistHeader(
                    bandName: item.band.name,
                    venue: item.performance.venue,
                    date: item.performance.date,
                    isFavorite: favorite,
                    downloadable: isPlayable,
                    downloadProgress: progress,
                    reviewsDestination: Navigation.reviews(ReviewsDestination(archiveIdentifier: item.recording.identifier)),
                    sourceInfoDestination: Navigation.sourceInfo(SourceInfoDestination(archiveIdentifier: item.recording.identifier)),
                    onFavoriteClick: { onFavoriteClick(item) },
                    onDownloadClick: { onDownloadClick(item) }
                )
            }
            
            switch progress {
            case .downloading(let bytesWritten, let bytesExpected, let timeRemaining):
                Section {
                    DownloadProgressView(
                        bytesWritten: bytesWritten,
                        bytesExpected: bytesExpected,
                        estimatedSecondsRemaining: timeRemaining
                    )
                }
                
            default:
                EmptyView()
            }
            
            Section {
                ForEach(item.tracks, id: \.id) { track in
                    TrackRow(
                        index: track.track,
                        title: track.title,
                        length: track.length,
                        downloading: false,
                        playing: isTrackPlaying(id: track.id),
                        disabled: !isPlayable
                    )
                    .onTapGesture { onTrackClick(item, track) }
                }
            }
        }
    }
    
    func isTrackPlaying(id: String) -> Bool {
        guard let playlist = app.audioSystem.playlist else { return false }
        return playlist.currentTrack.id == id
    }
    
    func isTrackDownloading(fileName: String) -> Bool {
        return downloadItems[fileName].map { !$0.finished } ?? false
    }
}
