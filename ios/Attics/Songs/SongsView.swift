////
////  SongsView.swift
////  Attics
////
////  Created by Zachary Wood on 12/11/19.
////  Copyright © 2019 Zachary Wood. All rights reserved.
////
//
//import SwiftUI
//import FontAwesome
//
//struct SongsView: View {
//    @ObservedObject var model: SongsViewModel
//    @ObservedObject var musicPlayer: MusicPlayer
//    @ObservedObject var downloadManager: DownloadManager
//    
//    var body: some View {
//        var a: Double? = nil
//        if downloadManager.progresses.count > 0 {
//            a = Double(downloadManager.progresses.count) / Double(model.songs.count)
//        }
//        
//        return ScrollView {
//            LocationView(
//                venue: model.source.show.venue,
//                city: model.source.show.city,
//                state: model.source.show.state,
//                isFavorite: model.isFavorite,
//                isDownloaded: downloadManager.downloaded,
//                percentageDownloaded: a,
//                onMoreInfoTap: model.onMoreInfoTap,
//                onFavoriteTap: model.onFavoriteTap,
//                onDownloadTap: {
//                    do {
//                        try self.downloadManager.download(songs: self.model.songs)
//                    } catch {
//                        print(error)
//                    }
//                }
//            )
//                .padding(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
//            SongList(model: model, musicPlayer: musicPlayer, downloadManager: downloadManager)
//                .padding(EdgeInsets(top: 0, leading: 24, bottom: 0, trailing: 24))
//        }.onAppear { self.loadSongs() }
//    }
//    
//    func loadSongs() {
//        if downloadManager.downloaded {
//            model.songs = downloadManager.downloadedSongs
//        } else {
//            print("loading songs")
//            model.loadData()
//        }
//    }
//}
//
//struct SongList: View {
//    @ObservedObject var model: SongsViewModel
//    @ObservedObject var musicPlayer: MusicPlayer
//    @ObservedObject var downloadManager: DownloadManager
//    
//    var body: some View {
//        VStack(spacing: 12) {
//            ForEach(model.indices, id: \.self) { index in
//                VStack(spacing: 12) {
//                    SongListItem(
//                        index: index,
//                        song: self.model.songs[index],
//                        isPlaying: self.musicPlayer.indexPlaying == index && self.musicPlayer.currentSong.source == self.model.source,
//                        onTap: { index in
//                            self.model.onSongTap(index, self.model.songs)
//                        },
//                        downloadProgress: self.downloadManager.progresses[self.model.songs[index]]
//                    )
//                    Divider()
//                }
//            }
//        }
//    }
//}
//
//struct SongListItem: View {
//    let index: Int
//    let song: Song
//    let isPlaying: Bool
//    let onTap: (Int) -> ()
//    let downloadProgress: Double?
//    
//    var body: some View {
//        Button(action: { self.onTap(self.index) }) {
//            HStack {
//                Text(String(self.index + 1)).font(.footnote)
//                Text(self.song.title)
//                    .padding(EdgeInsets(top: 0, leading: 4, bottom: 0, trailing: 0))
//                Spacer()
//                if self.downloadProgress != nil {
//                    Circle()
//                        .trim(from: 0.0, to: CGFloat(self.downloadProgress!))
//                        .stroke(Color.blue, lineWidth: 3)
//                        .frame(width: 14, height: 14)
//                } else if self.isPlaying {
//                    Text(String.fontAwesomeIcon(name: .music))
//                        .font(.init(UIFont.fontAwesome(ofSize: 14, style: .solid)))
//                } else {
//                    Text(self.song.length).font(.footnote)
//                }
//            }.foregroundColor(Color.primary)
//        }
//    }
//}
//
//struct LocationView: View {
//    let venue: String
//    let city: String
//    let state: String
//    let isFavorite: Bool
//    let isDownloaded: Bool
//    let percentageDownloaded: Double?
//    
//    let onMoreInfoTap: () -> ()
//    let onFavoriteTap: () -> ()
//    let onDownloadTap: () -> ()
//    
//    var body: some View {
//        HStack {
//            VStack(alignment: .leading) {
//                Text(venue).lineLimit(1)
//                Text("\(city), \(state)").lineLimit(1)
//            }.truncationMode(.tail)
//            Spacer(minLength: 16)
//            if percentageDownloaded != nil {
//                ActivityIndicator(isAnimating: .constant(true), style: .medium)
//            }
//            else if isDownloaded {
//                Icon(.cloudDownloadAlt, size: 24, padding: EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 8))
//                    .foregroundColor(Color.init(#colorLiteral(red: 0, green: 0.4660990834, blue: 0.2602835298, alpha: 1)))
//            } else {
//                Icon(.cloudDownloadAlt, size: 24, padding: EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 8))
//                    .onTapGesture(perform: onDownloadTap)
//            }
//            Icon(.heart, size: 24, padding: EdgeInsets(top: 0, leading: 8, bottom: 0, trailing: 8))
//                .onTapGesture(perform: onFavoriteTap)
//                .foregroundColor(isFavorite ? Color.red : Color.white)
//            Icon(.ellipsisH, size: 24, padding: EdgeInsets(top: 0, leading: 8, bottom: 0, trailing: 0)).onTapGesture(perform: onMoreInfoTap)
//        }
//        .padding(16)
//        .background(Color.init(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
//        .cornerRadius(8)
//        .foregroundColor(.white)
//        .shadow(color: .gray, radius: 2, x: 0, y: 0)
//        
//    }
//}
//
//struct SongsView_Previews: PreviewProvider {
//    
//    static var previews: some View {
//        NavigationView {
//            SongsView(model: TestModels.songsViewModel, musicPlayer: MusicPlayer.shared, downloadManager: DownloadManager(source: TestModels.testSource))
//                .navigationBarTitle("1977-05-08")
//        }
//    }
//}
//
//struct TestModels {
//    static let testYear = Year(collection: "Grateful Dead", year: "1977")
//    static let testShow = Show(collection: "Grateful Dead", date: "1977-05-08", venue: "Barton Hall", city: "Ithica", state: "NY", numReviews: 200, numSources: 1, avgRating: 4.5, year: testYear)
//    static let testSource = Source(identifier: "gd77-05-08.sbd.hicks.4982.sbeok.shnf", collection: "Grateful Dead", transferer: "Charlie Miller", source: "SBD", avgRating: 4.3, downloads: 100, numReviews: 20, lineage: "SBD>SBD", isFavorite: false, show: testShow)
//    static let testSongs = (0..<30).map { n in
//        Song(title: "Track \(n)", fileName: "track\(n).mp3", album: nil, track: 0, length: "1:00", source: testSource)
//    }
//    
//    static let songsViewModel = SongsViewModel(
//        source: testSource,
//        apiClient: WebAPIClient(),
//        favoritesStore: FileSystemFavoritesStore(),
//        onSongTap: { _,_ in },
//        onMoreInfoTap: {},
//        onFavoriteTap: {}
////        onDownloadTap: {}
//    )
//    
//    static let songsViewModelWithSongs: SongsViewModel = {
//        var vm = SongsViewModel(
//            source: testSource,
//            apiClient: WebAPIClient(),
//            favoritesStore: FileSystemFavoritesStore(),
//            onSongTap: { _,_ in },
//            onMoreInfoTap: {},
//            onFavoriteTap: {}
////            onDownloadTap: {}
//        )
//        vm.songs = testSongs
//        return vm
//    }()
//}
//
//
//struct ActivityIndicator: UIViewRepresentable {
//    
//    @Binding var isAnimating: Bool
//    let style: UIActivityIndicatorView.Style
//    
//    func makeUIView(context: UIViewRepresentableContext<ActivityIndicator>) -> UIActivityIndicatorView {
//        return UIActivityIndicatorView(style: style)
//    }
//    
//    func updateUIView(_ uiView: UIActivityIndicatorView, context: UIViewRepresentableContext<ActivityIndicator>) {
//        isAnimating ? uiView.startAnimating() : uiView.stopAnimating()
//    }
//}
//
//
//
