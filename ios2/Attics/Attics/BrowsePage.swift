//
//  BrowseView.swift
//  Attics
//
//  Created by Zachary Wood on 12/14/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import CosmosUI
import SwiftUI
import AtticsCore

struct BrowsePage: View {
    let bandId: String
    let bandName: String
    
    @State var result: APIResult<TopPerformancesPage> = .loading
    
    var body: some View {
        ResultView(result) { result in
            BrowseView(bandId: bandId, topPerformancesPage: result)
        }
        .atticsNavigationBar(bandName)
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
        .refreshable { await load() }
    }
    
    private func load() async {
        do {
            let data = try await app.apiClient.getTopPerformances(bandId: bandId)
            result = .success(data)
        } catch {
            logger.error("Failed to fetch BrowsePage for Band(id: \(bandId), name: \(bandName)): \(error)")
            result = .error(error)
        }
    }
}

struct BrowseView: View {
    var bandId: String
    var topPerformancesPage: TopPerformancesPage
    
    var body: some View {
        ScrollView(.vertical) {
            LazyVStack(alignment: .leading) {
                VStack(alignment: .leading) {
                    Text("On This Day").font(.title2).fontWeight(.bold)
                        .padding([.leading, .trailing], 16)
                    
                    BrowsePerformancesList(performances: topPerformancesPage.onThisDay)
                        .overlay {
                            if topPerformancesPage.onThisDay.isEmpty {
                                Text("No shows played on this date.")
                                    .font(.callout)
                            }
                        }
                }
                .tint(.clear)
                .padding([.top, .bottom], 12)
                
                ForEach(topPerformancesPage.years, id: \.year) { year in
                    VStack(alignment: .leading) {
                        NavigationLink(value: Navigation.year(YearDestination(bandId: bandId, year: year.year))) {
                            BrowseHeaderView(header: year.year)
                                .contentShape(Rectangle())
                        }
                        .buttonStyle(PlainButtonStyle())
                        .padding([.leading, .trailing], 16)
                        
                        BrowsePerformancesList(performances: year.topPerformances)
                    }
                    .contentShape(Rectangle())
                    .tint(.clear)
                    .padding([.top, .bottom], 12)
                }
            }
        }
    }
}

struct BrowseHeaderView: View {
    let header: String
    
    var body: some View {
        HStack {
            Text(header).font(.title2).fontWeight(.bold)
            Spacer()
            Text("See all")
                .font(.footnote).foregroundColor(Color(UIColor.secondaryLabel))
            Image(systemName: "chevron.right")
                .font(.footnote)
                .fontWeight(.light)
        }
    }
}

struct BrowsePerformancesList: View {
    let performances: [PerformanceWithMetadata]
    
    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack {
                ForEach(performances, id: \.id) { performance in
                    PerformancePreviewCard(performance: performance)
                }
            }
        }.frame(minHeight: 120)
    }
}

struct PerformancePreviewCard: View {
    @ScaledMetric(relativeTo: .body)
    var maxWidth = 160
    
    let performance: PerformanceWithMetadata
    
    var body: some View {
        NavigationLink(value: navigationValue) {
            VStack(alignment: .leading) {
                CosmosView(rating: performance.avgRating)
                
                Spacer(minLength: 38)
                Text(performance.date).font(.headline).foregroundColor(.white)
                Text(performance.venue).font(.footnote).foregroundColor(Color(UIColor.lightGray))
            }
            .contentShape(Rectangle())
            .padding([.leading, .trailing], 8)
            .padding([.top, .bottom], 12)
            .frame(minWidth: 130, maxWidth: maxWidth, alignment: .leading)
            .background(Color.atticsBlue)
            .cornerRadius(8)
        }
    }
    
    var navigationValue: Navigation {
        Navigation.performance(
            PerformanceDestination(
                performanceId: performance.id, performanceDate: performance.date
            )
        )
    }
}
