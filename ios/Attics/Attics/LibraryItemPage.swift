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
    var recordingId: String
    
    @State var result: APIResult<Library.Item> = .loading
    @State var showNetworkAlert = false
    
    @StateObject var downloads = app.downloads
    @StateObject var favorites = app.favorites
    @StateObject var audioSystem = app.audioSystem
    @EnvironmentObject var networkMonitor: NetworkMonitor
    
    var body: some View {
        ResultView(result) { result in
            LibraryItemView(
                item: result,
                favorite: favorites.favorited(recordingId: result.recording.id),
                progress: progress(item: result),
                downloadItems: downloads.fileDownloadProgresses,
                isPlayable: isPlayable(result),
                onFavoriteClick: onFavoriteClick,
                onDownloadClick: onDownloadClick,
                onTrackClick: onTrackClick
            )
        }
        .atticsNavigationBar()
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ShareLink(item: "https://attics.io/recordings/\(recordingId)")
        }
        .task { await load() }
        .refreshable { Task { await load() } }
        .alert(isPresented: $showNetworkAlert) {
            Alert(
                title: Text("Unable to play track"),
                message: Text("Network connection appears to be offline.")
            )
        }
    }
    
    func progress(item: Library.Item) -> DownloadProgress {
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
    
    private func onFavoriteClick(item: Library.Item) {
        do {
            if favorites.favorited(recordingId: item.recording.id) {
                try favorites.unfavorite(recordingId: item.recording.id)
            } else {
                try favorites.favorite(recordingId: item.recording.id)
            }
            
            UIDevice.vibrate(style: .medium)
        } catch {
            logger.error("Failed to favorite LibraryItem(recordingId: \(item.recording.id)): \(error)")
        }
    }
    
    private func onDownloadClick(item: Library.Item) {
        switch progress(item: item) {
        case .notDownloaded:
            guard isPlayable(item) else {
                showNetworkAlert = true
                return
            }
            
            let batchDownload = BatchDownload(identifier: item.recording.identifier, fileNames: item.tracks.map(\.fileName))
            let downloader = downloads.addDownloader(recordingId: item.recording.id, identifier: item.recording.identifier)
            downloader.download(batchDownload: batchDownload)
        case .downloading:
            downloads.cancelDownloader(identifier: item.recording.identifier)
        case .downloaded:
            downloads.removeDownload(id: item.recording.id)
        }
        
        UIDevice.vibrate(style: .medium)
    }
    
    private func isPlayable(_ item: Library.Item) -> Bool {
        if networkMonitor.status != .disconnected {
            return true
        }
        
        return downloads.downloadedRecordingIds.contains(item.recording.id)
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
    
    private func onTrackClick(_ item: Library.Item, _ track: Track) {
        guard isPlayable(item) else {
            showNetworkAlert = true
            return
        }
        
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
    }
}

struct LibraryItemView: View {
    var item: Library.Item
    var favorite: Bool
    var progress: DownloadProgress
    var downloadItems: [String : DownloadItem]
    var isPlayable: Bool
    var onFavoriteClick: (Library.Item) -> Void
    var onDownloadClick: (Library.Item) -> Void
    var onTrackClick: (Library.Item, Track) -> Void
    
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
                    reviewsDestination: LibraryNavigation.reviews(ReviewsDestination(archiveIdentifier: item.recording.identifier)),
                    sourceInfoDestination: LibraryNavigation.sourceInfo(SourceInfoDestination(archiveIdentifier: item.recording.identifier)),
                    onFavoriteClick: { onFavoriteClick(item) },
                    onDownloadClick: { onDownloadClick(item) }
                )
            }
            
            Section {
                switch progress {
                case .downloading(let bytesWritten, let bytesExpected, let timeRemaining):
                    DownloadProgressView(bytesWritten: bytesWritten, bytesExpected: bytesExpected, estimatedSecondsRemaining: timeRemaining)
                default:
                    EmptyView()
                }
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
                    .onTapGesture {
                        onTrackClick(item, track)
                    }
                }
            }
        }
    }
    
    func isTrackPlaying(id: String) -> Bool {
        guard let playlist = app.audioSystem.playlist else { return false }
        return playlist.currentTrack.id == id
    }
    
    func isTrackDownloading(fileName: String) -> Bool {
        let result = downloadItems[fileName].map { !$0.finished } ?? false
        logger.debug("\(fileName) is downloading: \(result)")
        return result
    }
}

struct DownloadProgressView: View {
    let bytesWritten: Int64
    let bytesExpected: Int64
    let estimatedSecondsRemaining: TimeInterval
    
    private static let fileByteCountFormatter: ByteCountFormatter = {
        let bcf = ByteCountFormatter()
        bcf.allowedUnits = [.useMB]
        bcf.countStyle = .file
        return bcf
    }()
    
    var body: some View {
        let percentage: Double = bytesExpected > 0 ? Double(bytesWritten) / Double(bytesExpected) : 0
        let bytesWrittenString = Self.fileByteCountFormatter.string(fromByteCount: bytesWritten)
        let bytesExpectedString = Self.fileByteCountFormatter.string(fromByteCount: bytesExpected)
        
        VStack(alignment: .leading) {
            ProgressView(value: percentage)
            Text("\(bytesWrittenString) / \(bytesExpectedString)")
                .font(.footnote)
                .fontWeight(.light)
        }
    }
}
