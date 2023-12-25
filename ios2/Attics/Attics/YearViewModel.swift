//
//  BrowseViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import Foundation
import Combine
import AtticsCore

class YearViewModel: ObservableObject {
    let app: AtticsCore
    let bandId: String
    let year: String
    
    @Published var performances: APIResult<[PerformanceWithMetadata]> = .loading
    var cancellable: AnyCancellable?
    
    init(app: AtticsCore, bandId: String, year: String) {
        self.app = app
        self.bandId = bandId
        self.year = year
    }
    
    func load() {
        cancellable = app.apiClient.getYearPerformances(bandId: bandId, year: year)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { [weak self] completion in
                switch completion {
                case .finished:
                    break
                case .failure(let error):
                    self?.performances = .error(error)
                }
            }, receiveValue: { [weak self] performances in
                self?.performances = .success(performances)
            })
    }
}
