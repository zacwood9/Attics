//
//  PlaylistHeader.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import SwiftUI

enum DownloadProgress {
    case notDownloaded
    case downloading(Int64, Int64, Double)
    case downloaded
}

struct PlaylistHeader: View {
    let bandName: String
    let venue: String
    let date: String
    let isFavorite: Bool
    let downloadable: Bool
    let downloadProgress: DownloadProgress
    
    let onFavoriteClick: () -> Void
    let onDownloadClick: () -> Void
    
    var body: some View {
        HStack {
            VStack(alignment: .leading) {
                Text(bandName).font(.title).fontWeight(.bold)
                Text(venue).font(.body)
                Text(date).font(.body)
            }
            
            Spacer()
        }
        .padding()
        .foregroundColor(.white)
        .background(Color.atticsBlue)
        .listRowInsets(EdgeInsets())
        
        HStack(spacing: 8) {
            Image(systemName: isFavorite ? "heart.slash.fill" : "heart")
                .foregroundColor(.red)
            Text(isFavorite ? "Remove from Library" : "Add to Library")
            Spacer()
        }
        .contentShape(Rectangle())
        .onTapGesture {
            onFavoriteClick()
        }
        
        HStack(spacing: 8) {
            switch downloadProgress {
            case .notDownloaded:
                Group {
                    Image(systemName: "arrow.down.circle")
                        .foregroundColor(.green)
                    Text("Download")
                    Spacer()
                }.foregroundStyle(downloadable ? .primary : Color.gray)
            case .downloading:
                ProgressView().progressViewStyle(.circular)
                Text("Cancel download")
                Spacer()
            case .downloaded:
                Image(systemName: "arrow.down.circle.fill")
                    .foregroundColor(.green)
                Text("Remove download")
                Spacer()
            }
        }
        
        .contentShape(Rectangle())
        .onTapGesture {
            onDownloadClick()
        }
    }
    
}

#Preview {
    List {
        Section {
            PlaylistHeader(
                bandName: "Grateful Dead", venue: "Barton Hall", date: "1977-05-08", isFavorite: true, downloadable: false, downloadProgress: .downloading(50, 100, 126), onFavoriteClick: { }, onDownloadClick: { }
            )
            
            PlaylistHeader(
                bandName: "Grateful Dead", venue: "Barton Hall", date: "1977-05-08", isFavorite: true, downloadable: false, downloadProgress: .notDownloaded, onFavoriteClick: { }, onDownloadClick: { }
            )
            
            PlaylistHeader(
                bandName: "Grateful Dead", venue: "Barton Hall", date: "1977-05-08", isFavorite: false, downloadable: false, downloadProgress: .downloaded, onFavoriteClick: { }, onDownloadClick: { }
            )
        }
    }
}
