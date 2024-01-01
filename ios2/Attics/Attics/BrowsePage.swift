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
    
    @State var result: APIResult<[YearWithTopPerformances]> = .loading
    
    var body: some View {
        ResultView(result) { result in
            BrowseView(bandId: bandId, yearsWithTopPerformances: result)
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
    @ScaledMetric(relativeTo: .body)
    var maxWidth = 160
    
    var bandId: String
    var yearsWithTopPerformances: [YearWithTopPerformances]
    
    var body: some View {
        ScrollView(.vertical) {
            LazyVStack(alignment: .leading) {
                ForEach(yearsWithTopPerformances, id: \.year) { year in
                    VStack(alignment: .leading) {
                        NavigationLink(value: Navigation.year(YearDestination(bandId: bandId, year: year.year))) {
                            HStack {
                                Text(year.year).font(.title2).fontWeight(.bold)
                                Spacer()
                                Text("See all")
                                    .font(.footnote).foregroundColor(Color(UIColor.secondaryLabel))
                                Image(systemName: "chevron.right")
                                    .font(.footnote)
                                    .fontWeight(.light)
                            }.contentShape(Rectangle())
                        }
                        .buttonStyle(PlainButtonStyle())
                        .padding([.leading, .trailing], 16)
                        
                        ScrollView(.horizontal, showsIndicators: false) {
                            HStack {
                                ForEach(year.topPerformances, id: \.date) { show in
                                    NavigationLink(value: Navigation.performance(PerformanceDestination(performanceId: show.id, performanceDate: show.date))) {
                                        VStack(alignment: .leading) {
                                            CosmosView(rating: show.avgRating)
                                            
                                            Spacer(minLength: 38)
                                            Text(show.date).font(.headline).foregroundColor(.white)
                                            Text(show.venue).font(.footnote).foregroundColor(Color(UIColor.lightGray))
                                        }
                                        .contentShape(Rectangle())
                                        .padding([.leading, .trailing], 8)
                                        .padding([.top, .bottom], 12)
                                        .frame(minWidth: 130, maxWidth: maxWidth, alignment: .leading)
                                        .background {
                                            Color.atticsBlue
                                        }
                                        .cornerRadius(8)
                                    }
                                }
                            }
                        }
                    }
                    .contentShape(Rectangle())
                    .tint(.clear)
                    .padding([.top, .bottom], 12)
                }
            }
        }
    }
}
