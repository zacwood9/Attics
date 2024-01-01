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
    let clearHistory: () -> ()
    let openUpdate: () -> ()
    
    @State var deleteOpen = false
    
    var body: some View {
        List {
            Section {
                header
            }.listRowInsets(.none)
                .listRowBackground(EmptyView())
            
            Section {
                list1()
            }
            
            Section {
                Button("Delete all downloads", role: .destructive) {
                    removeAllDownloads()
                }
            }
            
            Section {
                Button("Clear listening history", role: .destructive) {
                    clearHistory()
                }
            }
            
            Section {
                archive
            }
            
            Section {
                me
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
            Text("Version 2.0.0")
                .font(.title)
                .bold()
            Text("By: Zac Wood")
                .font(.title3)
        }
    }
    
    @ViewBuilder
    func list1() -> some View {
            HStack(spacing: 0) {
                Image(systemName: "star.square.fill")
                Text("Leave a review on the App Store!")
                    .padding(.leading)
                Spacer()
                Image(systemName: "chevron.right").font(.subheadline).fontWeight(.light)
            }.contentShape(Rectangle())
                .onTapGesture(perform: openReview)
            
            HStack(spacing: 0) {
                Image(systemName: "hammer.circle")
                Text("See the source code on GitHub")
                    .padding(.leading)
                Spacer()
                Image(systemName: "chevron.right").font(.subheadline).fontWeight(.light)
            }.contentShape(Rectangle())
                .onTapGesture { openSafari("https://github.com/zacwood9/Attics") }
            
            HStack(spacing: 0) {
                Image(systemName: "paperplane.circle.fill")
                Text("Suggestions? Send me a message")
                    .padding(.leading)
                Spacer()
                Image(systemName: "chevron.right").font(.subheadline).fontWeight(.light)
            }.contentShape(Rectangle())
                .onTapGesture { openSafari("mailto:zac.wood@hey.com") }
            
            HStack(spacing: 0) {
                Image(systemName: "arrowtriangle.up.circle")
                Text("View last update popup")
                    .padding(.leading)
                Spacer()
                Image(systemName: "chevron.right").font(.subheadline).fontWeight(.light)
            }.contentShape(Rectangle())
                .onTapGesture { openUpdate() }
            
            HStack(spacing: 0) {
                Image(systemName: "newspaper.fill")
                Text("Live Music Archive streaming policy")
                    .padding(.leading)
                Spacer()
                Image(systemName: "chevron.right").font(.subheadline)
            }.contentShape(Rectangle())
                .onTapGesture { openSafari("https://help.archive.org/hc/en-us/articles/360016553532-Live-Music-Archive-etree-org-#about-lma") }
        
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
            .background(Color.atticsBlue)
            .cornerRadius(8)
            
        }
    }
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        SettingsView(openSafari: { _ in}, openReview: { }, removeAllDownloads: { }, clearHistory: {}, openUpdate: { })
    }
}
