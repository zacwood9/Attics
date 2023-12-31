//
//  LibraryView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI

struct ResultView<T: Decodable, Content: View>: View {
    let result: APIResult<T>
    @ViewBuilder let content: (_ data: T) -> Content
    
    init(_ result: APIResult<T>, content: @escaping (_: T) -> Content) {
        self.result = result
        self.content = content
    }
    
    var body: some View {
        switch result {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(content(t)).id("library")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct LibraryPage: View {
    @State var result: APIResult<[Library.Item]> = .loading
    
    var body: some View {
        ResultView(result) { items in
            LoadedLibraryView(items: items)
                .refreshable { Task { await load() } }
        }
        .atticsNavigationBar("My Library")
        .task { await load() }
        .onReceive(app.favorites.objectWillChange, perform: { _ in
            Task { await load() }
        })
        .onReceive(app.downloads.$downloadedRecordingIds, perform: { _ in
            Task { await load() }
        })
    }
    
    func load() async {
        do {
            result = .success(try await app.library.loadItems())
        } catch {
            result = .error(error)
        }
    }
}

struct LoadedLibraryView: View {
    let items: [Library.Item]
    @EnvironmentObject var downloads: Downloads
    @EnvironmentObject var favorites: Favorites
    
    var groups: [String : [Library.Item]] {
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
    func itemView(_ item: Library.Item) -> some View {
        NavigationLink(value: LibraryNavigation.recording(RecordingDestination(recordingId: item.recording.id))) {
            HStack {
                VStack(alignment: .leading) {
                    Text(item.performance.date).fontWeight(.bold)
                    Text(item.performance.venue).font(.footnote).lineLimit(1).fontWeight(.light)
                }
                
                Spacer()
                if downloads.downloadedRecordingIds.contains(item.recording.id) {
                    Image(systemName: "square.and.arrow.down.fill")
                        .foregroundColor(.green)
                }
                if favorites.favorited(recordingId: item.recording.id) {
                    Image(systemName: "heart.fill")
                        .foregroundColor(.red)
                }
            }
        }
    }
}
