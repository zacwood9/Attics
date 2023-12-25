//
//  MyView.swift
//  Attics
//
//  Created by Zachary Wood on 12/14/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import SwiftUI

struct RecordingView2: View {
    var songs = ["Mr. Charlie", "Fire on the Mountain", "Hello World"]
    
    var body: some View {
        VStack {
            List() {
                Section {
                    HStack {
                        VStack(alignment: .leading) {
                            Text("Billy Strings").font(.title).fontWeight(.bold)
                            Text("Rex Theatre, Fairfax, VA").font(.body)
                            Text("2022-05-07").font(.body)
                        }
                        
                        Spacer()
                    }
                    .padding()
                    .foregroundColor(.white)
                    .background(Color(UIColor.atticsBlue))
                    .listRowInsets(EdgeInsets())
                    HStack {
                        Image(systemName: "heart")
                            .foregroundColor(Color(UIColor.atticsBlue))
                        Text("Add to My Shows")
                    }
                    HStack {
                        Image(systemName: "square.and.arrow.down")
                            .foregroundColor(Color(UIColor.atticsBlue))
                        Text("Download")
                    }
                    HStack {
                        Image(systemName: "info.square.fill").foregroundColor(Color(UIColor.atticsBlue))
                        Text("Taper's Notes")
                        Spacer()
                        Image(systemName: "chevron.right")
                            .font(.footnote)
                    }
                    HStack {
                        Image(systemName: "text.bubble.fill")
                            .foregroundColor(Color(UIColor.atticsBlue))
                        Text("Reviews")
                        Spacer()
                        Image(systemName: "chevron.right")
                            .font(.footnote)
                    }
                }
                
                Section {
                    ForEach(songs.indices, id: \.self) { i in
                        HStack {
                            Text("\(i + 1). ").fontWeight(.light).font(.footnote)
                            Text(songs[i])
                            Spacer()
                            Text("05:37").fontWeight(.light).font(.footnote)
                        }
                    }
                }
        
            }
        }
        
    }
}

struct RecordingView2_Previews: PreviewProvider {
    static var previews: some View {
        RecordingView2()
    }
}
