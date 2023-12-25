//
//  YearView2.swift
//  Attics
//
//  Created by Zachary Wood on 12/15/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import SwiftUI

let shows = [
        ShowT(date: "1977-05-09", venue: "Times Union Center"),
        ShowT(date: "1977-05-10", venue: "Barton Hall"),
        ShowT(date: "1977-05-11", venue: "Barton Hall"),
        ShowT(date: "1977-05-12", venue: "Barton Hall"),
        ShowT(date: "1977-05-13", venue: "Barton Hall"),
        ShowT(date: "1977-05-09", venue: "Barton Hall"),
        ShowT(date: "1977-05-10", venue: "Barton Hall"),
        ShowT(date: "1977-05-11", venue: "Barton Hall"),
        ShowT(date: "1977-05-12", venue: "Barton Hall"),
        ShowT(date: "1977-05-13", venue: "Barton Hall"),
    ]


struct YearView2: View {
    var body: some View {
        List(shows, id: \.date) { show in
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .top) {
                    VStack(alignment: .leading) {
                        Text(show.venue)
                            .font(.subheadline)
                            .foregroundColor(Color(UIColor.lightGray))
                        Text("Albany, NY")
                            .font(.subheadline)
                            .foregroundColor(Color(UIColor.lightGray))
                    }.font(.footnote)
                    Spacer()
                    VStack(alignment: .trailing, spacing: 0) {
                        RatingView(rating: 4.3)
                        Text(show.venue)
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
            .listRowInsets(EdgeInsets(top: 4, leading: 8, bottom: 4, trailing: 8))
            .padding(8)
            .background(Color(UIColor.atticsBlue))
            .cornerRadius(8)
            .listRowSeparator(.hidden)
        }
        .listStyle(.inset)
    }
}

struct YearView2_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            YearView2()
                .navigationTitle("1977")
        }
        
    }
}
