////
////  SongsViewModel.swift
////  Attics
////
////  Created by Zachary Wood on 12/22/19.
////  Copyright © 2019 Zachary Wood. All rights reserved.
////
//
//import SwiftUI
//
//class SongsViewModel: ObservableObject {
//    @Published var songs = [Song]()
//    @Published var isFavorite: Bool
//    
//    var indices: Range<Int> {
//        0..<songs.count
//    }
//    
//    let source: Source
//    let apiClient: APIClient
//    let favoriteStore: FavoritesStore
//    let onSongTap: (Int, [Song]) -> ()
//    let onMoreInfoTap: () -> ()
//    let onFavoriteTap: () -> ()
//    
////    func onDownloadTap() {
////        do {
////            try downloadManager.download(source: source, songs: songs)
////        } catch {
////            print(error)
////        }
////    }
//    
//    init(source: Source, apiClient: APIClient, favoritesStore: FavoritesStore,
//         onSongTap:     @escaping (Int, [Song]) -> () = { _,_ in },
//         onMoreInfoTap: @escaping () -> () = {},
//         onFavoriteTap: @escaping () -> () = {}
////         onDownloadTap: @escaping () -> () = {}
//    ) {
//        self.source = source
//        self.apiClient = apiClient
//        self.favoriteStore = favoritesStore
//        self.onSongTap = onSongTap
//        self.onMoreInfoTap = onMoreInfoTap
//        self.onFavoriteTap = onFavoriteTap
////        self.onDownloadTap = onDownloadTap
//        self.isFavorite = favoritesStore.contains(source: source)
//        
//        NotificationCenter.default.addObserver(self, selector: #selector(refreshFavorite), name: .FavoriteWasAdded, object: nil)
//        NotificationCenter.default.addObserver(self, selector: #selector(refreshFavorite), name: .FavoriteWasRemoved, object: nil)
//    }
//    
//    @objc func refreshFavorite() {
//        isFavorite = favoriteStore.isFavorite(source: source)
//    }
//    
//    func loadData() {
//        apiClient.fetchSongs(in: source) { result in
//            switch result {
//            case .success(let songs):
//                DispatchQueue.main.async {
//                    self.songs = songs                    
//                }
//            default:
//                print("oops")
//            }
//        }
//    }
//}
