////
////  NowPlayingView.swift
////  Attics
////
////  Created by Zachary Wood on 12/22/19.
////  Copyright © 2019 Zachary Wood. All rights reserved.
////
//
//import SwiftUI
//
//struct NowPlayingView: View {
//    @ObservedObject var vm: SongsViewModel
//    @ObservedObject var musicPlayer: MusicPlayer
//    @ObservedObject var downloadManager: DownloadManager
//    
//    var body: some View {
//        ZStack {
//            Color.init(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)).edgesIgnoringSafeArea(.all)
//                            
//            VStack {
//                LargeTitle(text: vm.source.show.date).background(Color.init(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
//                
//                LocationView(
//                    venue: vm.source.show.venue,
//                    city: vm.source.show.city,
//                    state: vm.source.show.state,
//                    isFavorite: vm.isFavorite,
//                    isDownloaded: downloadManager.downloaded,
//                    percentageDownloaded: nil,
//                    onMoreInfoTap: vm.onMoreInfoTap,
//                    onFavoriteTap: vm.onFavoriteTap,
//                    onDownloadTap: {
//                        do {
//                            try self.downloadManager.download(songs: self.vm.songs)
//                        } catch {
//                            print(error)
//                        }
//                    }
//                ).padding(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
//                
//                ScrollView {
//                    SongList(model: vm, musicPlayer: musicPlayer, downloadManager: downloadManager)
//                        .padding(EdgeInsets(top: 0, leading: 24, bottom: 0, trailing: 24))
//                }
//                
//                VStack {
//                    self.TimeSeeker
//                    self.ButtonBar
//                }
//                .padding(EdgeInsets(top: 0, leading: 0, bottom: 16, trailing: 0))
//                .background(Color.init(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
//            }
//            .background(Color.primary.colorInvert())
//            .navigationBarTitle(vm.source.show.date)
//        }
//    }
//    
//    var TimeSeeker: some View {
//        VStack {
//            Slider(
//                value: self.$musicPlayer.percentageSongFinished,
//                in: 0...1,
//                onEditingChanged: { start in
//                    if start {
//                        self.musicPlayer.seeking = true
//                    } else {
//                        self.musicPlayer.seek(percentage: self.musicPlayer.percentageSongFinished)
//                        self.musicPlayer.seeking = false
//                    }
//            }
//            ).padding(EdgeInsets(top: 12, leading: 24, bottom: 0, trailing: 24))
//            
//            HStack {
//                Text(self.musicPlayer.seeking
//                    ? (self.musicPlayer.percentageSongFinished * self.musicPlayer.currentDurationInSeconds).timeString
//                    : self.musicPlayer.currentTimeInSeconds.timeString).foregroundColor(Color.white)
//                Spacer()
//                Text(self.musicPlayer.currentDurationInSeconds.timeString).foregroundColor(Color.white)
//            }
//            .padding(EdgeInsets(top: 0, leading: 24, bottom: 0, trailing: 24))
//        }
//    }
//    
//    var ButtonBar: some View {
//        HStack(alignment: .center, spacing: 36) {
//            Spacer()
//            Icon(.backward, size: 48, padding: EdgeInsets())
//                .onTapGesture {
//                    self.musicPlayer.playPreviousTrack()
//            }.foregroundColor(Color.white)
//            Icon(musicPlayer.status == .playing ? .pause : .play, size: 48, padding: EdgeInsets())
//                .onTapGesture {
//                    if self.musicPlayer.status == .playing {
//                        self.musicPlayer.pause()
//                    } else {
//                        self.musicPlayer.resume()
//                    }
//            }.foregroundColor(Color.white)
//            Icon(.forward, size: 48, padding: EdgeInsets())
//                .onTapGesture {
//                    self.musicPlayer.playNextTrack()
//            }.foregroundColor(Color.white)
//            Spacer()
//        }
//    }
//    
//    init(_ vm: SongsViewModel, _ musicPlayer: MusicPlayer, _ downloadManager: DownloadManager) {
//        self.vm = vm
//        self.musicPlayer = musicPlayer
//        self.downloadManager = downloadManager
//    }
//}
//
//struct LargeTitle: View {
//    let text: String
//    
//    var body: some View {
//        HStack {
//            Text(text)
//                .font(.largeTitle).fontWeight(.bold).foregroundColor(Color.white)
//                .padding(EdgeInsets(top: 36, leading: 12, bottom: 12, trailing: 0))
//            Spacer()
//        }
//    }
//}
//
//#if DEBUG
//struct NowPlayingView_Previews: PreviewProvider {
//    static var previews: some View {
//        NowPlayingView(TestModels.songsViewModelWithSongs, MusicPlayer.shared, DownloadManager(source: TestModels.testSource))
//            .environment(\.colorScheme, .dark)
//    }
//}
//#endif
