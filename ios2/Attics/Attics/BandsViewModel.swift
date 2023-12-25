//
//  BrowseViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import Foundation
import Combine
import AtticsCore


class BandsViewModel: ObservableObject {
    let app: AtticsCore
    
    @Published var bands: APIResult<[BandWithMetadata]> = .loading
    var bandsCancellable: AnyCancellable?
    
    init(app: AtticsCore) {
        self.app = app
    }
    
    @MainActor func load() async {
        do {
            let bands = try await app.apiClient.getBands()
            self.bands = .success(bands)
        } catch {
            self.bands = .error(error)
        }
    }
}
