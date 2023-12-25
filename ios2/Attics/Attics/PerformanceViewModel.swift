//
//  BrowseViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import Foundation
import Combine
import AtticsCore

class PerformanceViewModel: ObservableObject {
    let app: AtticsCore
    let performanceId: String
    let performanceDate: String
    
    @Published var recordings: APIResult<[APIRecording]> = .loading
    var cancellable: AnyCancellable?
    
    init(app: AtticsCore, performanceId: String, performanceDate: String) {
        self.app = app
        self.performanceId = performanceId
        self.performanceDate = performanceDate
    }
    
    func load() {
        cancellable = app.apiClient.getPerformance(performanceId: performanceId)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { [weak self] completion in
                switch completion {
                case .finished:
                    break
                case .failure(let error):
                    self?.recordings = .error(error)
                }
            }, receiveValue: { [weak self] recordings in
                self?.recordings = .success(recordings)
            })
    }
}
