//
//  BrowseTab.swift
//  Attics
//
//  Created by Zachary Wood on 12/29/23.
//

import SwiftUI

struct BrowseTab: View {
    @State var destinations = [Navigation]()
    
    var body: some View {
        NavigationStack(path: $destinations) {
            BandsPage()
                .navigationDestination(for: Navigation.self) { destination in
                    switch destination {
                    case .band(let bandDestination):
                        BrowseView(bandId: bandDestination.bandId, bandName: bandDestination.bandName)
                    case .year(let yearDestination):
                        YearView(bandId: yearDestination.bandId, year: yearDestination.year)
                    case .performance(let destination):
                        PerformanceView(performanceId: destination.performanceId, performanceDate: destination.performanceDate)
                    case .recording(let destination):
                        RecordingView(recordingId: destination.recordingId)
                    }
                    
                }
        }
    }
}
