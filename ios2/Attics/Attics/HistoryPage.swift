//
//  HistoryPage.swift
//  Attics
//
//  Created by Zachary Wood on 12/30/23.
//

import SwiftUI
import AtticsCore

struct HistoryPage: View {
    @State var result: APIResult<[History.Item]> = .loading
    
    var body: some View {
        ResultView(result) { items in
            HistoryView(items: items)
        }
        .atticsNavigationBar("Listening history")
        .onAppear {
            Task.detached { await self.load() }
        }
        .refreshable {
            Task.detached { await self.load() }
        }
        .onReceive(app.history.objectWillChange, perform: { _ in
            Task.detached { await self.load() }
        })
    }
    
    private func load() async {
        do {
            let items = try app.history.getItems()
            await MainActor.run { self.result = .success(items) }
        } catch {
            logger.error("Failed to fetch history items: \(error)")
            await MainActor.run { self.result = .error(error) }
        }
    }
}

struct HistoryView: View {
    let items: [History.Item]
    
    var groups: [(String, [History.Item])] {
        Dictionary(
            grouping: items,
            by: { item in
                let result = Calendar.current.dateComponents([.year, .month, .day], from: item.playedAt)
                return result
            }
        )
        .compactMap { pair in
            let (date, items) = pair
            guard let year = date.year, let month = date.month, let day = date.day else { return nil }
            return ("\(year)-\(month)-\(day)", items)
        }
        .sorted { $0.0 > $1.0 }
    }
    
    var body: some View {
        List(groups, id: \.0) { group in
            Section(group.0) {
                ForEach(group.1, id: \.id) { item in
                    NavigationLink(value: LibraryNavigation.recording(RecordingDestination(recordingId: item.recordingId))) {
                        VStack(alignment: .leading) {
                            Text(item.trackTitle).fontWeight(.semibold)
                            Text("\(item.bandName), \(item.date)")
                                .font(.footnote)
                        }
                    }
                }
            }
            
        }
    }
}

