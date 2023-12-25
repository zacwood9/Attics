//
//  LibraryItemView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI

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
            AnyView(LoadedLibraryItemView(viewModel: viewModel, loadedItem: t)).id("library")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct LoadedLibraryItemView: View {
    @ObservedObject var viewModel: LibraryItemViewModel
    
    let loadedItem: LoadedLibraryItem
    var item: LibraryItem { loadedItem.item }
    
    var body: some View {
        List {
            Section {
                PlaylistHeader(bandName: item.band.name, venue: item.performance.venue, date: item.performance.date, isFavorite: false, onFavoriteClick: onFavoriteClick)
            }
            
            Section {
                ForEach(loadedItem.tracks, id: \.id) { track in
                    Text(track.title)
                }
            }
        }
    }
    
    func onFavoriteClick() {
        let favorites = viewModel.app.favorites
        
        do {
            if favorites.favorited(recordingId: item.recording.id) {
                try favorites.unfavorite(recordingId: item.recording.id)
            } else {
                
            }
        } catch {
            print(error)
        }
    }
}
