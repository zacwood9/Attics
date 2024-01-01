//
//  UpdateView.swift
//  Attics
//
//  Created by Zachary Wood on 12/31/23.
//

import SwiftUI

struct UpdateView: View {
    let dismiss: () -> ()
    
    var body: some View {
        ScrollView {
            VStack(alignment: .center, spacing: 16) {
                
                ZStack {
                    VStack {
                        Image("logo")
                            .resizable()
                            .scaledToFit()
                            .frame(maxHeight: 250)
                            .padding([.leading, .trailing], 16)
                        Text("New update!")
                            .font(.title)
                            .bold()
                        Text("What's new?")
                            .font(.title3)
                    }
                    
                    VStack {
                        HStack {
                            Spacer()
                            Image(systemName: "xmark.circle.fill")
                                .foregroundColor(.white)
                                .font(.title)
                                .padding(.trailing,  24)
                                .padding(.top, 8)
                                .onTapGesture {
                                    dismiss()
                                }
                        }
                        Spacer()
                    }
                }
                .padding(.top, 16)
                
                design.padding()
                history.padding()
                feedback.padding()
            }
        }
    }
    
    var design: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("New design")
                .font(.title2)
                .bold()
            
            Text("Attics has been rebuilt from the ground up, and most screens and the navigation structure have been redesigned. The biggest change is the My Library page, which now shows all your saved shows across all bands.")
        }
    }
    
    var history: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("Listening history")
                .font(.title2)
                .bold()
            
            Text("Trying to remember what show that Dark Star was from last week? See all the tracks you've listened to on the new Listening History page, accessible from the My Library tab.")
        }
    }
    
    var feedback: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("Feedback")
                .font(.title2)
                .bold()
            
            Text("I'd love to hear your feedback on the new design! Please contact me on the 'More' tab with any feedback or ideas for how to improve the app.")
        }
    }
}

#Preview {
    UpdateView(dismiss: {})
}
