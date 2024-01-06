//
//  LibraryTab.swift
//  Attics
//
//  Created by Zachary Wood on 12/29/23.
//

import SwiftUI

struct LibraryTab: View {
    var path: Binding<[LibraryNavigation]>
    
    var body: some View {
        NavigationStack(path: path) {
            LibraryPage()
                .navigationDestination(for: LibraryNavigation.self) { destination in
                    switch destination {
                    case .recording(let destination):
                        LibraryItemPage(recordingId: destination.recordingId)
                    case .history:
                        HistoryPage()
                    case .reviews(let destination):
                        ReviewsPage(archiveIdentifier: destination.archiveIdentifier)
                    case .sourceInfo(let destination):
                        SourceInfoPage(archiveIdentifier: destination.archiveIdentifier)
                    }
                    
                }
        }
    }
}
