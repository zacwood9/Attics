//
//  BrowseView.swift
//  Attics
//
//  Created by Zachary Wood on 12/14/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import SwiftUI

struct ShowT {
    let date: String
    let venue: String
}

struct TopShowsT {
    let year: String
    let shows: [ShowT]
}

let records = [
    TopShowsT(year: "1977", shows: [
        ShowT(date: "1977-05-08", venue: "Barton Hall"),
        ShowT(date: "1977-05-09", venue: "Barton Hall"),
        ShowT(date: "1977-05-10", venue: "Barton Hall"),
        ShowT(date: "1977-05-11", venue: "Barton Hall"),
        ShowT(date: "1977-05-12", venue: "Barton Hall"),
    ]),
    TopShowsT(year: "1978", shows: [
        ShowT(date: "1977-05-09", venue: "Barton Hall, Barton Hall"),
        ShowT(date: "1977-05-10", venue: "Barton Hall"),
        ShowT(date: "1977-05-11", venue: "Barton Hall"),
        ShowT(date: "1977-05-12", venue: "Barton Hall"),
        ShowT(date: "1977-05-13", venue: "Barton Hall"),
        ShowT(date: "1977-05-09", venue: "Barton Hall"),
        ShowT(date: "1977-05-10", venue: "Barton Hall"),
        ShowT(date: "1977-05-11", venue: "Barton Hall"),
        ShowT(date: "1977-05-12", venue: "Barton Hall"),
        ShowT(date: "1977-05-13", venue: "Barton Hall"),
    ])
]

struct BrowseView: View {
    @ScaledMetric(relativeTo: .body)
    var maxWidth = 160
    
    var body: some View {
        List(records, id: \.year) { topShows in
            VStack(alignment: .leading) {
                HStack {
                    Text(topShows.year).font(.title2).fontWeight(.bold)
                    Spacer()
                    Text("See all >")
                     .font(.footnote).foregroundColor(Color(UIColor.secondaryLabel))
                }.padding([.leading, .trailing], 16)
                
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack {
                        ForEach(topShows.shows, id: \.date) { show in
                            VStack(alignment: .leading) {
                                RatingView(rating: 3.9)

                                    
                                Spacer(minLength: 38)
                                Text(show.date).font(.headline).foregroundColor(.white)
                                Text(show.venue).font(.footnote).foregroundColor(Color(UIColor.lightGray))
                            }
                            
                            .padding([.leading, .trailing], 8)
                            .padding([.top, .bottom], 12)
                            .frame(minWidth: 130, maxWidth: maxWidth, alignment: .leading)
                            .background(Color(UIColor.atticsBlue))
                            .cornerRadius(8)
                        }
                    }
                }
            }
            .padding([.top, .bottom], 12)
            
            .listRowInsets(EdgeInsets())
        }
        .listStyle(.plain)
        .navigationTitle("Grateful Dead")
    }
}

struct BrowseView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BrowseView()
        }
    }
}
