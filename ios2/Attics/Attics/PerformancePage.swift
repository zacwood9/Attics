//
//  YearView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import CosmosUI
import SwiftUI
import AtticsCore

struct PerformancePage: View {
    let performanceId: String
    let performanceDate: String
    
    @State var result: APIResult<[APIRecording]> = .loading
    
    var body: some View {
        ResultView(result) { result in
            PerformanceView(performanceDate: performanceDate, recordings: result)
        }
            .atticsNavigationBar("\(performanceDate) recordings")
            
    }
    
    var mainView: some View {
        switch performanceViewModel.recordings {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView().id("performances")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct PerformanceView: View {
    var performanceDate: String
    var recordings: [APIRecording]
    
    var body: some View {
        List(recordings, id: \.id) { recording in
            BetterNavigationLink(value: Navigation.recording(RecordingDestination(recordingId: recording.id))) {
                VStack(alignment: .leading, spacing: 12) {
                    HStack(alignment: .top) {
                        VStack(alignment: .leading) {
                            Text(recording.source)
                                .lineLimit(1)
                                .font(.subheadline)
                                .foregroundColor(Color(UIColor.lightGray))
                            Text("\(recording.archiveDownloads) download\(recording.archiveDownloads == 1 ? "" : "s")")
                                .font(.subheadline)
                                .foregroundColor(Color(UIColor.lightGray))
                        }.font(.footnote)
                        Spacer()
                        VStack(alignment: .trailing, spacing: 0) {
                            CosmosView(rating: recording.avgRating)
                            Text("\(recording.numReviews) review\(recording.numReviews == 1 ? "" : "s")")
                                .font(.subheadline).foregroundColor(Color(UIColor.lightGray))
                        }
                    }
                    
                    HStack {
                        Text(recording.transferer)
                            .font(.title2)
                            .fontWeight(.bold)
                            .foregroundColor(.white)
                    }
                }                
            }
            .listRowInsets(EdgeInsets(top: 4, leading: 8, bottom: 4, trailing: 8))
            .padding(8)
            .background(Color.atticsBlue)
            .cornerRadius(8)
            .listRowSeparator(.hidden)
        }
        .listStyle(.inset)
    }
}
