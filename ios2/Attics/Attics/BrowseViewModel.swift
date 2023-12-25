//
//  BrowseViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import Foundation
import Combine
import AtticsCore

class BrowseViewModel: ObservableObject {
    let app: AtticsCore
    let bandId: String
    let bandName: String
    
    @Published var yearsWithTopPerformances: APIResult<[YearWithTopPerformances]> = .loading
    var cancellable: AnyCancellable?
    
    init(app: AtticsCore, bandId: String, bandName: String) {
        self.app = app
        self.bandId = bandId
        self.bandName = bandName
    }
    
    func load() {
        cancellable = app.apiClient.getTopPerformances(bandId: bandId)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { [weak self] completion in
                switch completion {
                case .finished:
                    break
                case .failure(let error):
                    self?.yearsWithTopPerformances = .error(error)
                }
            }, receiveValue: { [weak self] yearsWithTopPerformances in
                self?.yearsWithTopPerformances = .success(yearsWithTopPerformances)
            })
    }
}
