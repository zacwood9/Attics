////
////  ContentView.swift
////  Attics
////
////  Created by Zachary Wood on 10/28/20.
////
//
//import SwiftUI
//
//struct PlaylistView: View {
//    @StateObject var viewModel: NowPlayingViewModel
//    @StateObject var player: MusicPlayer
//    
//    @State private var showShareSheet = false
//    
//    var body: some View {
//        switch viewModel.state {
//        case .idle, .loading:
//            Text("").onAppear(perform: viewModel.load)
//        case .failed(let error):
//            Text(error.localizedDescription)
//        case .loaded(let output):
//            content(output)
//        }
//    }
//    
//    func content(_ output: NowPlayingViewModel.Response) -> some View {
//        ScrollView {
//            VStack {
//                PlayerHeader(viewModel: viewModel, player: player)
//                Divider()
//                SongList(songClick: songClick, songs: output.songs.mp3s)
//                    .environmentObject(player)
//                    .environmentObject(viewModel)
//            }
//            .navigationBarItems(
//                trailing: Image(systemName: "square.and.arrow.up")
//                    .foregroundColor(.white)
//                    .contentShape(Rectangle())
//                    .onTapGesture {
//                        showShareSheet = true
//                    }
//            )
//        }.sheet(isPresented: $showShareSheet) {
//            let band = viewModel.band
//            let show = viewModel.show
//            let source = viewModel.source
//            let url = URL(string: "https://attics.io/\(band.collection)/\(show.year)/\(show.date)/\(source.identifier)")!
//            ShareSheet(activityItems: [url])
//        }
//    }
//    
//    func songClick(song: Song) {
//        switch viewModel.state {
//        case .loaded(let response):
//            player.dispatch(
//                action: .play(
//                    song,
//                    Playlist(
//                        band: viewModel.band,
//                        show: viewModel.show,
//                        source: viewModel.source,
//                        songs: response.songs.mp3s
//                    )
//                )
//            )
//        default:
//            fatalError("asdf")
//        }
//    }
//}
//
//struct PlayerHeader : View {
//    @StateObject var viewModel: NowPlayingViewModel
//    @StateObject var player: MusicPlayer
//    
//    var body: some View {
//        VStack {
//            HStack {
//                Spacer()
//                VStack {
//                    Text(viewModel.band.name)
//                        .font(.title)
//                        .bold()
//                    if viewModel.show.venue != "Unknown" {
//                        Text("Live at \(viewModel.show.venue)")
//                            .font(.title2)
//                    }
//                    
//                    Text(viewModel.show.date)
//                        .font(.title3)
//                }
//                Spacer()
//            }.padding(EdgeInsets(top: 12, leading: 0, bottom: 0, trailing: 0))
//            
//            HStack {
//                FavoriteButton(viewModel: viewModel)
//                DownloadButton(viewModel: viewModel)
//            }
//        }
//    }
//}
//
//struct FavoriteButton: View {
//    @StateObject var viewModel: NowPlayingViewModel
//    @EnvironmentObject var favorites: FavoritesStore
//    
//    var body: some View {
//        print("body")
//        return Button(action: {
//            print(favorites.file.name)
//            if favorites.contains(source: viewModel.source) {
//                favorites.remove(source: viewModel.source)
//            } else {
//                favorites.save(source: viewModel.source)
//            }
//        }) {
//            HStack {
//                Image(systemName: "heart")
//                Text("Favorite")
//            }
//        }
//        .foregroundColor(.white)
//        .padding()
//        .background(favorites.contains(source: viewModel.source) ? Color.red : Color.accentColor)
//        .cornerRadius(8)
//        .padding()
//    }
//}
//
//struct DownloadButton: View {
//    @StateObject var viewModel: NowPlayingViewModel
//    
//    var body: some View {
//        Button(action: { print("b") }) {
//            HStack {
//                Image(systemName: "square.and.arrow.down")
//                Text("Download")
//            }
//        }
//        .foregroundColor(.white)
//        .padding()
//        .background(Color.accentColor)
//        .cornerRadius(8)
//        .padding()
//    }
//}
////
////struct Playlist_Previews: PreviewProvider {
////    static var previews: some View {
////        Group {
////            DownloadButton(viewModel: NowPlayingViewModel()
////        }.previewLayout(.sizeThatFits)
////    }
////}
