//
//  YearView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import CosmosUI
import SwiftUI
import AtticsCore

struct PerformanceView: View {
    @StateObject var performanceViewModel: PerformanceViewModel
    
    init(performanceId: String, performanceDate: String) {
        self._performanceViewModel = StateObject(wrappedValue: PerformanceViewModel(app: app, performanceId: performanceId, performanceDate: performanceDate))
    }
    
    var body: some View {
        mainView
            .toolbarBackground(Color.atticsBlue, for: .navigationBar)
            .toolbarBackground(.visible, for: .navigationBar)
            .toolbarColorScheme(.dark, for: .navigationBar)
            .navigationTitle("\(performanceViewModel.performanceDate) recordings")
            .onAppear { performanceViewModel.load() }
    }
    
    var mainView: some View {
        switch performanceViewModel.recordings {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(PerformanceList(performanceDate: performanceViewModel.performanceDate, recordings: t)).id("performances")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct PerformanceList: View {
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
                            Text("\(recording.archiveDownloads) downloads")
                                .font(.subheadline)
                                .foregroundColor(Color(UIColor.lightGray))
                        }.font(.footnote)
                        Spacer()
                        VStack(alignment: .trailing, spacing: 0) {
                            CosmosView(rating: recording.avgRating)
                            Text("\(recording.numReviews) reviews\(recording.numReviews > 1 ? "s" : "")")
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
