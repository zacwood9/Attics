//
//  YearView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import CosmosUI
import SwiftUI
import AtticsCore

struct YearView: View {
    @StateObject var yearViewModel: YearViewModel
    
    init(bandId: String, year: String) {
        self._yearViewModel = StateObject(wrappedValue: YearViewModel(app: app, bandId: bandId, year: year))
    }
    
    var body: some View {
        mainView
            .backgroundStyle(Color(UIColor.systemGroupedBackground))
            .toolbarBackground(Color.atticsBlue, for: .navigationBar)
            .toolbarBackground(.visible, for: .navigationBar)
            .toolbarColorScheme(.dark, for: .navigationBar)
            .navigationTitle(yearViewModel.year)
            .onAppear { yearViewModel.load() }
    }
    
    var mainView: some View {
        switch yearViewModel.performances {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .success(let t):
            AnyView(YearList(performances: t)).id("performances")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        }
    }
}

struct YearList: View {
    var performances: [PerformanceWithMetadata]
    
    var body: some View {
        List(performances, id: \.id) { show in
            BetterNavigationLink(value: Navigation.performance(PerformanceDestination(performanceId: show.id, performanceDate: show.date))) {
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
                }                
            }
            .listRowInsets(EdgeInsets(top: 4, leading: 8, bottom: 4, trailing: 8))
            .padding(8)
            .background(Color.atticsBlue)
            .cornerRadius(8)
            .listRowSeparator(.hidden)
        }
        .listStyle(.inset)
        .backgroundStyle(Color(UIColor.systemGroupedBackground))
    }
}
