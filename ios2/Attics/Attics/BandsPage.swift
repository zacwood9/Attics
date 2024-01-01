//
//  BandsView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import SwiftUI
import AtticsCore


struct BandsPage: View {
    @Environment(\.openURL) var openURL
    
    @State var result: APIResult<[BandWithMetadata]> = .loading
    @State var searchText: String = ""
    
    var body : some View {
        ResultView(result) { bands in
            BandsView(
                bands: bands
                    .filter { filterResults($0) }
                    .sorted { $0.name < $1.name },
                suggestion: suggestion
            )
        }
        .atticsNavigationBar("Attics")
        .task {
            switch result {
            case .success:
                break
            case .error:
                result = .loading
                await load()
            case .loading:
                await load()
            }
        }
        .refreshable { Task { await load() } }
        .searchable(text: $searchText, placement: .navigationBarDrawer(displayMode: .automatic), prompt: "Search bands")
    }
    
    private func load() async {
        do {
            let bands = try await app.apiClient.getBands()
            result = .success(bands)
        } catch {
            result = .error(error)
        }
    }
    
    private func filterResults(_ band: BandWithMetadata) -> Bool {
        guard !searchText.isEmpty else { return true }
        
        let matchesName = band.name.lowercased().contains(searchText.lowercased())
        return matchesName
    }
    
    private func suggestion() {
        if let url = URL(string: "mailto:zac.wood@hey.com") {
            openURL(url)
        }
        
    }
}

struct BandsView: View {
    var bands: [BandWithMetadata]
    var suggestion: () -> Void
    
    var body: some View {
        List {
            Section("Bands") {
                ForEach(bands, id: \.id) { band in
                    NavigationLink(value: Navigation.band(BandDestination(bandId: band.id, bandName: band.name))) {
                        HStack {                            
                            RemoteImage(name: band.name, url: URL(string: band.logoUrl)!)
                                .frame(width: 48, height: 48)
                                .clipped()
                                .cornerRadius(8)

                            VStack(alignment: .leading) {
                                Text(band.name).fontWeight(.bold)
                                Text("\(band.numPerformances) shows")
                                    .font(.subheadline)
                            }
                        }
                    }
                }
            }
            
            Section("Suggestions") {
                HStack {
                    Image(systemName: "paperplane.circle.fill")
                    Text("Suggest a band")
                }.contentShape(Rectangle())
                    .onTapGesture {
                        suggestion()
                    }
            }
        }
    }
}
