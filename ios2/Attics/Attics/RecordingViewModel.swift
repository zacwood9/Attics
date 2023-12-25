//
//  RecordingViewModel.swift
//  Attics
//
//  Created by Zachary Wood on 12/21/23.
//

import Foundation
import AtticsCore
import Combine

class RecordingViewModel: ObservableObject, Observable {
    let app: AtticsCore
    let recordingId: String
    
    @Published var recordingView: APIResult<APIRecordingPage> = .loading
    var cancellable: AnyCancellable?
    
    init(app: AtticsCore, recordingId: String) {
        self.app = app
        self.recordingId = recordingId
    }
    
    func load() {
        cancellable = app.apiClient.getRecording(recordingId: recordingId)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { [weak self] completion in
                switch completion {
                case .finished:
                    break
                case .failure(let error):
                    self?.recordingView = .error(error)
                }
            }, receiveValue: { [weak self] recordingView in
                self?.recordingView = .success(recordingView)
            })
    }
}
