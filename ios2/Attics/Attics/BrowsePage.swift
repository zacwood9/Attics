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

struct BrowseView: View {
    let bandId: String
    let bandName: String
    
    @State var result: APIResult<[YearWithTopPerformances]> = .loading
    
    var body: some View {
        ResultView(result) { result in
            BrowseList(bandId: bandId, yearsWithTopPerformances: result)
        }
        .atticsNavigationBar(bandName)
        .task { await load() }
        .refreshable { Task { await load() } }
    }
    
    private func load() async {
        
    }
}

struct BrowseList: View {
    @ScaledMetric(relativeTo: .body)
    var maxWidth = 160
    
    var bandId: String
    var yearsWithTopPerformances: [YearWithTopPerformances]
    
    var body: some View {
        List(yearsWithTopPerformances, id: \.year) { year in
            VStack(alignment: .leading) {
                NavigationLink(value: Navigation.year(YearDestination(bandId: bandId, year: year.year))) {
                    HStack {
                        Text(year.year).font(.title2).fontWeight(.bold)
                        Spacer()
                        Text("See all")
                            .font(.footnote).foregroundColor(Color(UIColor.secondaryLabel))
                    }
                    
                }
                .padding([.leading, .trailing], 16)
                
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack {
                        ForEach(year.topPerformances, id: \.date) { show in
                            VStack(alignment: .leading) {
                                CosmosView(rating: show.avgRating)
                                
                                Spacer(minLength: 38)
                                Text(show.date).font(.headline).foregroundColor(.white)
                                Text(show.venue).font(.footnote).foregroundColor(Color(UIColor.lightGray))
                            }
                            .padding([.leading, .trailing], 8)
                            .padding([.top, .bottom], 12)
                            .frame(minWidth: 130, maxWidth: maxWidth, alignment: .leading)
                            .overlay {
                                NavigationLink(value: Navigation.performance(PerformanceDestination(performanceId: show.id, performanceDate: show.date))) { Color.clear }
                            }
                            .background {
                                Color.atticsBlue
                            }
                            .cornerRadius(8)
                            
                        }
                    }
                }
            }
            .tint(.clear)
            .listItemTint(.clear)
            .padding([.top, .bottom], 12)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
        }
        .backgroundStyle(Color(UIColor.systemGroupedBackground))
        .listStyle(.inset)
    }
}
