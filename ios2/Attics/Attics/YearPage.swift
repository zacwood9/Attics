//
//  YearView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import CosmosUI
import SwiftUI
import AtticsCore

struct YearPage: View {
    enum SortKeys: String, CaseIterable, Hashable, Equatable {
        case dateAsc = "Date (asc)"
        case dateDesc = "Date (desc)"
        case totalStars = "Total Stars"
        case numRecordings = "Num. Recordings"
    }
    
    static let sortFuncs: [SortKeys : (PerformanceWithMetadata, PerformanceWithMetadata) -> Bool] = [
        .dateAsc: { $0.date < $1.date },
        .dateDesc: { $0.date > $1.date },
        .totalStars: { ($0.avgRating * Double($0.numReviews)) > ($1.avgRating * Double($1.numReviews)) },
        .numRecordings: { $0.numRecordings > $1.numRecordings }
    ]
    
    let bandId: String
    let year: String
    
    @State var result: APIResult<[PerformanceWithMetadata]> = .loading
    @State var searchText: String = ""
    @State var sortBy = SortKeys.dateAsc
    
    var body: some View {
        ResultView(result) { result in
            YearView(
                performances: result.filter { matchesSearch($0) }.sorted(by: Self.sortFuncs[sortBy]!)
            )
        }
        .atticsNavigationBar(year)
        .toolbar {
            Menu("Sort by: \(sortBy.rawValue)") {
                ForEach(SortKeys.allCases, id: \.self) { key in
                    Button(key.rawValue) { sortBy = key }
                }
            }
        }
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
        .searchable(text: $searchText, placement: .navigationBarDrawer(displayMode: .always), prompt: "Search by date, venue, location")
        
    }
    
    private func load() async {
        do {
            let data = try await app.apiClient.getYearPerformances(bandId: bandId, year: year)
            self.result = .success(data)
        } catch {
            logger.error("Failed to load YearPage(bandId: \(bandId), year: \(year)): \(error)")
            self.result = .error(error)
        }
    }
    
    private func matchesSearch(_ item: PerformanceWithMetadata) -> Bool {
        guard !searchText.isEmpty else { return true }
        
        let searchText = searchText.lowercased()
        let venueMatches: (PerformanceWithMetadata) -> Bool = { $0.venue.lowercased().contains(searchText) }
        let cityMatches: (PerformanceWithMetadata) -> Bool = { $0.city.lowercased().starts(with: searchText) }
        let stateMatches: (PerformanceWithMetadata) -> Bool = { $0.state.lowercased().starts(with: searchText) }
        let dateMatches: (PerformanceWithMetadata) -> Bool = { $0.date.contains(searchText) }
        
        return venueMatches(item) || cityMatches(item) || stateMatches(item) || dateMatches(item)
    }
}

struct YearView: View {
    var performances: [PerformanceWithMetadata]
    
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
                ForEach(performances, id: \.id) { show in
                    NavigationLink(value: Navigation.performance(PerformanceDestination(performanceId: show.id, performanceDate: show.date))) {
                        VStack(alignment: .leading, spacing: 12) {
                            HStack(alignment: .top) {
                                VStack(alignment: .leading) {
                                    Text(show.venue)
                                        .font(.subheadline)
                                        .foregroundColor(Color(UIColor.lightGray))
                                    Text(show.cityState)
                                        .font(.subheadline)
                                        .foregroundColor(Color(UIColor.lightGray))
                                }.font(.footnote)
                                Spacer()
                                VStack(alignment: .trailing, spacing: 0) {
                                    CosmosView(rating: show.avgRating)
                                    Text("\(show.numRecordings) recording\(show.numRecordings > 1 ? "s" : "")")
                                        .font(.subheadline).foregroundColor(Color(UIColor.lightGray))
                                }
                            }
                            
                            HStack {
                                Text(show.date)
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
