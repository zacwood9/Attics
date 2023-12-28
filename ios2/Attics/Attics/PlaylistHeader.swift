//
//  PlaylistHeader.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import SwiftUI

enum DownloadProgress {
    case notDownloaded
    case downloading
    case downloaded
}

struct PlaylistHeader: View {
    let bandName: String
    let venue: String
    let date: String
    let isFavorite: Bool
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
        
        HStack {
            Image(systemName: isFavorite ? "heart.slash.fill" : "heart")
                .foregroundColor(.red)
            Text(isFavorite ? "Remove from Library" : "Add to Library")
            Spacer()
        }
        .contentShape(Rectangle())
        .onTapGesture {
            onFavoriteClick()
        }
        
        HStack {
            switch downloadProgress {
            case .notDownloaded:
                Image(systemName: "square.and.arrow.down")
                    .foregroundColor(.green)
                Text("Download")
            case .downloading:
                Image(systemName: "square.and.arrow.down")
                    .foregroundColor(.green)
                Text("Cancel download")
            case .downloaded:
                Image(systemName: "square.and.arrow.down.fill")
                    .foregroundColor(.green)
                Text("Remove download")
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
                bandName: "Grateful Dead", venue: "Barton Hall", date: "1977-05-08", isFavorite: true, downloadProgress: .downloading, onFavoriteClick: { }, onDownloadClick: { }
            )
        }
    }
}
