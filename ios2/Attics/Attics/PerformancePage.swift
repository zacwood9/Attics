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
    static let sortFuncs: [String : (APIRecording, APIRecording) -> Bool] = [
        "Downloads": { $0.archiveDownloads > $1.archiveDownloads },
        "Avg. Rating": { $0.avgRating > $1.avgRating },
        "Num. Reviews": { $0.numReviews > $1.numReviews }
    ]
    
    let performanceId: String
    let performanceDate: String
    
    @State var result: APIResult<[APIRecording]> = .loading
    @State var sortBy = "Downloads"
    
    var body: some View {
        ResultView(result) { result in
            PerformanceView(
                performanceDate: performanceDate,
                recordings: result.sorted(by: Self.sortFuncs[sortBy] ?? { $0.archiveDownloads < $1.archiveDownloads })
            )
        }
            .atticsNavigationBar("\(performanceDate) recordings")
            .task {
                switch result {
                case .success:
                    break
                case .error:
                    result = .loading
                    await load()
                case .loading:
                    await load()
                }
            }
            .refreshable { Task { await load() } }
            .toolbar {
                Menu("Sort by: \(sortBy)") {
                    ForEach(Array(Self.sortFuncs.keys), id: \.self) { key in
                        Button(key) { sortBy = key }
                    }
                }
            }
            
    }
    
    private func load() async {
        do {
            let data = try await app.apiClient.getPerformance(performanceId: performanceId)
            self.result = .success(data)
        } catch {
            logger.error("Failed to load PerformancePage(performanceId: \(performanceId), performanceDate: \(performanceDate)): \(error)")
            self.result = .error(error)
        }
    }
}

struct PerformanceView: View {
    var performanceDate: String
    var recordings: [APIRecording]
    
    @Environment(\.horizontalSizeClass)
    var horizontalSizeClass
    
    var columns: [GridItem] {
        switch horizontalSizeClass {
        case .compact:
            return [GridItem()] // Using 2 columns if it's narrow
        default:
            return [GridItem(), GridItem()] // Using 4 columns if it's wide
        }
    }
    
    var body: some View {
        ScrollView {
            LazyVGrid(columns: columns, spacing: 8) {
                ForEach(recordings, id: \.id) { recording in
                    NavigationLink(value: Navigation.recording(RecordingDestination(recordingId: recording.id))) {
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
                        }.contentShape(Rectangle())
                    }
                    .buttonStyle(PlainButtonStyle())
                    .padding(8)
                    .background(Color.atticsBlue)
                    .cornerRadius(8)
                    .padding([.horizontal], 4)
                }
            }.padding([.top], 8)
        }
    }
}
