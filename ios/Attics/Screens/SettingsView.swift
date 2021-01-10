//
//  SettingsView.swift
//  Attics
//
//  Created by Zachary Wood on 1/8/21.
//  Copyright © 2021 Zachary Wood. All rights reserved.
//

import SwiftUI

struct SettingsView: View {
    let openSafari: (String) -> ()
    let openReview: () -> ()
    let removeAllDownloads: () -> ()
    @State var deleteOpen = false
    
    var body: some View {
        ScrollView {
            VStack {
                header
                Divider().padding(.top)
                
                list1()
                
                Divider()
                HStack(alignment: .bottom) {
                    Text("Delete all downloads")
                        .foregroundColor(.red)
                        .padding(.leading)
                    Spacer()
                }.contentShape(Rectangle())
                .onTapGesture {
                    deleteOpen = true
                }
                Divider()
                
                
                archive
                    .padding(.top, 32)
                    .padding([.leading, .trailing])
                
                me
                    .padding(.top, 32)
                    .padding([.leading, .trailing])

            }
        }.actionSheet(isPresented: $deleteOpen) {
            let nvm = ActionSheet.Button.cancel(Text("Never mind!")) { deleteOpen = false }
            let remove = ActionSheet.Button.destructive(Text("Yes, remove all downloads")) { removeAllDownloads() }
            return ActionSheet(title: Text("Remove all downloads?"), message: nil, buttons: [remove, nvm])
        }
    }
    
    var header: some View {
        VStack {
            Image("logo")
                .resizable()
                .scaledToFit()
                .frame(maxHeight: 250)
                .padding([.leading, .trailing], 16)
            Text("Version 1.5")
                .font(.title)
                .bold()
            Text("Developed by Zachary Wood")
                .font(.title3)
        }
    }
    
    @ViewBuilder
    func list1() -> some View {
        HStack {
            Text("Leave a review on the App Store!")
                .padding(.leading)
            Spacer()
        }.contentShape(Rectangle())
        .onTapGesture(perform: openReview)
        Divider()
        
        HStack {
            Text("See the source code on GitHub")
                .padding(.leading)
            Spacer()
        }.contentShape(Rectangle())
        .onTapGesture { openSafari("https://github.com/zacwood9/Attics") }
        Divider()
        
        HStack {
            Text("Suggestions? Send me a message")
                .padding(.leading)
            Spacer()
        }.contentShape(Rectangle())
        .onTapGesture { openSafari("mailto:zac.wood@hey.com") }
        Divider()
            .padding(.bottom)
    }
    
    var archive: some View {
        VStack(alignment: .center, spacing: 16) {
            Text("Attics is proudly powered by the Internet Archive's Live Music Archive. Please consider supporting their awesome work with a donation!").multilineTextAlignment(.center)
            
            
                Button(action: { openSafari("https://archive.org/donate/")}, label: {
                    Text("Donate to archive.org")
                        .padding()
                }).padding([.leading, .trailing], 8)
                .foregroundColor(.white)
                .background(Color(#colorLiteral(red: 0, green: 0.3046096265, blue: 0.1889052391, alpha: 1)))
                .cornerRadius(8)
        
        }
            
    }
    
    var me: some View {
        VStack(alignment: .center, spacing: 16) {
            Text("Running Attics's servers has monthly costs for me as well. To support me and future development work on Attics, feel free to leave a tip below, review the app, or just send a nice message 😊").multilineTextAlignment(.center)
            
            
            Button(action: { openSafari("https://paypal.me/atticstipjar")}, label: {
                Text("Zac's Tip Jar")
                    .padding()
            }).padding([.leading, .trailing], 8)
            .foregroundColor(.white)
            .background(Color(#colorLiteral(red: 0, green: 0.3046096265, blue: 0.1889052391, alpha: 1)))
            .cornerRadius(8)
            
        }
    }
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        SettingsView(openSafari: { _ in}, openReview: { }, removeAllDownloads: { })
    }
}
