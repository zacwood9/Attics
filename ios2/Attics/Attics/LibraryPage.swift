//
//  LibraryView.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import SwiftUI

struct LibraryPage: View {
    @State var result: APIResult<[Library.Item]> = .loading
    @State var searchText: String = ""
    
    @EnvironmentObject var networkMonitor: NetworkMonitor
    
    var body: some View {
        ResultView(result) { items in
            LibraryView(
                items: items.filter { matchesSearch($0) },
                itemDisabled: { !isPlayable($0) }
            )
            .refreshable { Task { await load() } }
            .searchable(
                text: $searchText,
                placement: .navigationBarDrawer(displayMode: .always),
                prompt: "Search by band, date, venue, location"
            )
        }
        .atticsNavigationBar("My Library")
        .task {
            guard case .loading = result else { return }
            await load()
        }
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
    
    private func matchesSearch(_ item: Library.Item) -> Bool {
        guard !searchText.isEmpty else { return true }
        
        let searchText = searchText.lowercased()
        let venueMatches: (Library.Item) -> Bool = { $0.performance.venue.lowercased().contains(searchText) }
        let cityMatches: (Library.Item) -> Bool = { $0.performance.city.lowercased().starts(with: searchText) }
        let stateMatches: (Library.Item) -> Bool = { $0.performance.state.lowercased().starts(with: searchText) }
        let dateMatches: (Library.Item) -> Bool = { $0.performance.date.contains(searchText) }
        let bandMatches: (Library.Item) -> Bool = { $0.band.name.lowercased().contains(searchText) }
        
        return bandMatches(item) || venueMatches(item) || cityMatches(item) || stateMatches(item) || dateMatches(item)
    }
    
    private func isPlayable(_ item: Library.Item) -> Bool {
        if networkMonitor.status != .disconnected {
            return true
        }
        
        return app.downloads.downloadedRecordingIds.contains(item.recording.id)
    }
}

struct LibraryView: View {
    var items: [Library.Item]
    var itemDisabled: (Library.Item) -> Bool
    
    var groups: [String : [Library.Item]] {
        Dictionary.init(grouping: items, by: \.band.name)
    }
    
    var body: some View {
        List {
            Section("My stuff") {
                NavigationLink(value: LibraryNavigation.history) {
                    Image(systemName: "clock.arrow.circlepath")
                    Text("Listening history")
                }
            }
            
            ForEach(groups.keys.sorted(), id: \.self) { key in
                let items = groups[key]!
                Section(items[0].band.name) {
                    ForEach(items.sorted(by: { $0.performance.date < $1.performance.date }), id: \.recording.id) { item in
                        ItemView(item: item, itemDisabled: itemDisabled)
                    }
                }
                
            }
        }
    }
}

struct ItemView: View {
    var item: Library.Item
    var itemDisabled: (Library.Item) -> Bool
    
    var value: LibraryNavigation {
        .recording(RecordingDestination(recordingId: item.recording.id))
    }
    
    var body: some View {
        NavigationLink(value: itemDisabled(item) ? nil : value) {
            HStack {
                VStack(alignment: .leading) {
                    Text(item.performance.date).fontWeight(.bold)
                    Text(item.performance.venue).font(.footnote).lineLimit(1).fontWeight(.light)
                }
                
                Spacer()
                if item.recording.downloaded {
                    Image(systemName: "arrow.down.circle.fill")
                        .foregroundColor(.green)
                }
                if item.recording.favorite {
                    Image(systemName: "heart.fill")
                        .foregroundColor(.red)
                }
            }
        }
    }
}
