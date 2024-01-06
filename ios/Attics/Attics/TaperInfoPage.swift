//
//  TaperInfoPage.swift
//  Attics
//
//  Created by Zachary Wood on 1/6/24.
//

import SwiftUI
import AtticsCore

struct SourceInfoPage: View {
    
    let archiveIdentifier: String
    @State private var result: APIResult<APIMetadataPage> = .loading
    
    var body: some View {
        ResultView(result) { page in
            SourceInfoView(metadata: page.result)
        }
        .atticsNavigationBar("Source Info")
        .task {
            do {
                let page = try await app.apiClient.getMetadata(archiveIdentifier: archiveIdentifier)
                self.result = .success(page)
            } catch {
                print(error)
                self.result = .error(error)
            }
        }
    }
}

fileprivate struct SourceInfoView: View {
    let metadata: APIMetadata
    
    @Environment(\.openURL) var openURL
    
    var url: URL? {
        URL(string: "https://archive.org/details/\(metadata.identifier)")
    }
    
    var body: some View {
        List {
            if let title = metadata.title {
                InfoRow(key: "Title", value: title)
            }
            
            if let description = metadata.description {
                InfoRow(key: "Description", value: description)
            }
            
            if let subject = metadata.subject {
                InfoRow(key: "Subject", value: subject)
            }
            
            if let venue = metadata.venue {
                InfoRow(key: "Venue", value: venue)
            }
            
            if let coverage = metadata.coverage {
                InfoRow(key: "Coverage", value: coverage)
            }
            
            if let source = metadata.source {
                InfoRow(key: "Source", value: source)
            }
            
            if let taper = metadata.taper {
                InfoRow(key: "Taper", value: taper)
            }
            
            if let transferer = metadata.transferer {
                InfoRow(key: "Transferer", value: transferer)
            }
            
            if let notes = metadata.notes {
                InfoRow(key: "Notes", value: notes)
            }
            
            InfoRow(key: "Idenfitier", value: metadata.identifier)
            
            if let addeddate = metadata.addeddate {
                InfoRow(key: "Uploaded At", value: addeddate)
            }            
            
            Section {
                Button("View on archive.org") {
                    if let url {
                        openURL.callAsFunction(url)
                    }
                }
                .foregroundStyle(.white)
                .buttonStyle(PlainButtonStyle())
            }.listRowBackground(Color.atticsBlue)
        }
    }
}

fileprivate struct InfoRow: View {
    let key: String
    let value: String
    
    var body: some View {
        Section(key) {
            Text(value)
        }
    }
}

#Preview {
    NavigationStack {
        SourceInfoPage(archiveIdentifier: "gd1983-06-28.135355.Unk-Hill.Keo.Flac2496")
    }
}
