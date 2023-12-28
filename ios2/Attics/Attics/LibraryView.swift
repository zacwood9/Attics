//
//  LibraryView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI

struct LibraryView: View {
    @StateObject var viewModel = LibraryViewModel(app: app)
    
    var body: some View {
        mainView
            .navigationTitle("My Library")
            .environmentObject(viewModel)
            .task {
                await viewModel.load()
            }
    }
    
    var mainView: some View {
        switch viewModel.items {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(LoadedLibraryView(items: t)).id("library")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct LoadedLibraryView: View {
    let items: [LibraryItem]
    @EnvironmentObject var viewModel: LibraryViewModel
    @EnvironmentObject var downloads: Downloads
    @EnvironmentObject var favorites: Favorites
    
    var groups: [String : [LibraryItem]] {
        Dictionary.init(grouping: items, by: \.band.name)
    }
    
    var body: some View {
        List(groups.keys.sorted(), id: \.self) { key in
            if let items = groups[key] {
                Section(items[0].band.name) {
                    ForEach(items.sorted(by: { $0.performance.date < $1.performance.date }), id: \.recording.id) { item in
                        itemView(item)
                    }
                }
            } else {
                EmptyView()
            }

        }
    }
    
    @ViewBuilder
    func itemView(_ item: LibraryItem) -> some View {
        NavigationLink(value: LibraryNavigation.recording(RecordingDestination(recordingId: item.recording.id))) {
            HStack {
                VStack(alignment: .leading) {
                    Text(item.performance.date).fontWeight(.bold)
                    Text(item.performance.venue).font(.footnote).lineLimit(1).fontWeight(.light)
                }
                            
                Spacer()
                if favorites.favorited(recordingId: item.recording.id) {
                    Image(systemName: "heart.fill")
                        .foregroundColor(.red)
                }
                if downloads.downloadedRecordingIds.contains(item.recording.id) {
                    Image(systemName: "square.and.arrow.down.fill")
                        .foregroundColor(.green)
                }
            }
        }
    }
}
