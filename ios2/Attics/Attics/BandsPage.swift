//
//  BandsView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import SwiftUI
import AtticsCore

struct BandsView: View {
    @State var bands: APIResult<[BandWithMetadata]> = .loading
    
    var body : some View {
        resolver
            .navigationTitle("Attics")
            .task {
                await refresh()
            }
    }
    
    
    var resolver: some View {
        switch bands {
        case .loading:
            return AnyView(ProgressView()).id("loading")
        case .error(let error):
            return AnyView(Text(error.localizedDescription)).id("error")
        case .success(let bands):
            let bandView = BandsList(
                bands: bands.sorted(by: { $0.name < $1.name }),
                refresh: refresh
            )
            
            return AnyView(bandView).id("bands")
        }
    }
    
    @Sendable
    func refresh() async {
        do {
            let bands = try await app.apiClient.getBands()
            self.bands = .success(bands)
        } catch {
            self.bands = .error(error)
        }
    }
}

struct BandsList: View {
    var bands: [BandWithMetadata]
    var refresh: @Sendable () async -> Void
    
    var body: some View {
        List {
            ForEach(bands, id: \.id) { band in
                NavigationLink(value: Navigation.band(BandDestination(bandId: band.id, bandName: band.name))) {
                    VStack(alignment: .leading) {
                        Text(band.name).fontWeight(.bold)
                        Text("\(band.numPerformances) shows")
                            .font(.subheadline)
                    }
                    
                }
            }
        }.refreshable(action: refresh)
    }
}
