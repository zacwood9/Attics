//
//  BrowseTab.swift
//  Attics
//
//  Created by Zachary Wood on 12/29/23.
//

import AtticsCore
import SwiftUI

struct HomeTab: View { 
    var path: Binding<[Navigation]>
    
    var body: some View {
        NavigationStack(path: path) {
            BandsPage()
                .navigationDestination(for: Navigation.self) { destination in
                    switch destination {
                    case .band(let bandDestination):
                        BrowsePage(bandId: bandDestination.bandId, bandName: bandDestination.bandName)
                    case .year(let yearDestination):
                        YearPage(bandId: yearDestination.bandId, year: yearDestination.year)
                    case .performance(let destination):
                        PerformancePage(performanceId: destination.performanceId, performanceDate: destination.performanceDate)
                    case .recording(let destination):
                        RecordingPage(recordingIdOrIdentifier: destination.recordingId)
                    case .storedRecording(let destination):
                        LibraryItemPage(recordingId: destination.recordingId)
                    }
                }
        }
    }
}
