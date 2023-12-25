//
//  PerformanceView2.swift
//  Attics
//
//  Created by Zachary Wood on 12/16/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import SwiftUI


struct PerformanceView2: View {
    var body: some View {
        List(shows, id: \.date) { show in
            VStack(alignment: .leading, spacing: 12) {
                HStack(alignment: .top) {
                    VStack(alignment: .leading) {
                        Text("Unknown Lineage")
                            .font(.subheadline)
                            .foregroundColor(Color(UIColor.lightGray))
                        Text("57283 Reviews")
                            .font(.subheadline)
                            .foregroundColor(Color(UIColor.lightGray))
                    }.font(.footnote)
                    Spacer()
                    VStack(alignment: .trailing, spacing: 0) {
                        RatingView(rating: 4.3)
                        Text("81237 Downloads")
                            .font(.subheadline).foregroundColor(Color(UIColor.lightGray))
                    }
                }
                
                HStack {
                    Text("Charlie Miller")
                        .font(.title2)
                        .fontWeight(.bold)
                        .foregroundColor(.white)
                    Spacer()
                    Text("AUD")
                        .padding(4)
                        .fontWeight(.bold)
                        .foregroundColor(.white)
                        .background(Color(#colorLiteral(red: 0, green: 0.3046096265, blue: 0.1889052391, alpha: 1)))
                        .cornerRadius(8)
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

struct PerformanceView2_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            PerformanceView2()
                .navigationTitle("1977")
        }
        
    }
}
