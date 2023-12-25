//
//  LibraryViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/24/23.
//

import AtticsCore
import Foundation
import Combine

struct LibraryItem: Decodable {
    let band: Band
    let performance: Performance
    let recording: Recording
}

class LibraryViewModel: ObservableObject {
    let app: AtticsCore
    @Published var items: APIResult<[LibraryItem]> = .loading
    
    var favoritesCancellable: AnyCancellable?
    
    init(app: AtticsCore) {
        self.app = app
        favoritesCancellable = app.favorites.objectWillChange.receive(on: DispatchQueue.main).sink { _ in
            Task { [weak self] in
                await self?.load()
            }
        }
    }
    
    @MainActor
    func load() async {
        do {
            items = .success(try app.favorites.loadFavorites().map { favorite in
                let (band, performance, recording) = favorite
                return LibraryItem(band: band, performance: performance, recording: recording)
            })
        } catch {
            items = .error(error)
        }
    }
}
