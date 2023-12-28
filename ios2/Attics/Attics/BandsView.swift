//
//  BandsView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import SwiftUI
import AtticsCore

struct BandsView: View {
    @ObservedObject var viewModel: BandsViewModel
        
    var body : some View {
        switch viewModel.bands {
        case .loading:
            AnyView(ProgressView()).id("loading")
        case .error(let error):
            AnyView(Text(error.localizedDescription)).id("error")
        case .success(let bands):
            AnyView(BandsList(bands: bands)).id("bands")
        }
    }
}

struct BandsList: View {
    var bands: [BandWithMetadata]
    
    var body: some View {
        List {
            ForEach(bands, id: \.id) { band in
                NavigationLink(value: Navigation.band(BandDestination(bandId: band.id, bandName: band.name)))
                {
                    VStack(alignment: .leading) {
                        Text(band.name).fontWeight(.bold)
                        Text("\(band.numPerformances) shows")
                            .font(.subheadline)
                    }
                    
                }
            }
        }
    }
}
