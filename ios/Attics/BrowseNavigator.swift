//
//  BrowseNavigator.swift
//  Attics
//
//  Created by Zachary Wood on 7/24/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Network
import CoreData
import Combine

enum RestoreState: Codable {
    case band(Band)
    case year(Band, Year)
    case performance(Band, Show)
    case recording(Band, Show, Source)
    
    private enum CodingKeys: String, CodingKey {
        case type
        case value
    }
    
    private struct YearPayload: Codable {
        let band: Band
        let year: Year
    }
    
    private struct PerformancePaylaod: Codable {
        let band: Band
        let performance: Show
    }
    
    private struct RecordingPayload: Codable {
        let band: Band
        let performance: Show
        let recording: Source
    }
    
    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: .type)
        
        switch type {
        case "band":
            self = .band(try container.decode(Band.self, forKey: .value))
        case "year":
            let payload = try container.decode(YearPayload.self, forKey: .value)
            self = .year(payload.band, payload.year)
        case "performance":
            let payload = try container.decode(PerformancePaylaod.self, forKey: .value)
            self = .performance(payload.band, payload.performance)
        case "recording":
            let payload = try container.decode(RecordingPayload.self, forKey: .value)
            self = .recording(payload.band, payload.performance, payload.recording)
        default: throw DecodingError.dataCorruptedError(forKey: .value, in: container, debugDescription: "unable to decode value")
        }
    }
    
    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .band(let band):
            try container.encode("band", forKey: .type)
            try container.encode(band, forKey: .value)
        case .year(let band, let year):
            try container.encode("year", forKey: .type)
            try container.encode(YearPayload(band: band, year: year), forKey: .value)
        case .performance(let band, let performance):
            try container.encode("performance", forKey: .type)
            try container.encode(PerformancePaylaod(band: band, performance: performance), forKey: .value)
        case .recording(let band, let performance, let recording):
            try container.encode("recording", forKey: .type)
            try container.encode(RecordingPayload(band: band, performance: performance, recording: recording), forKey: .value)
        }
    }
}

final class BrowseNavigator: AtticsNavigator {
    var state = AppState(id: "browse-v1.5")
    lazy var restoreState: RestoreState = storage.browseState
    let topPerfsVC: TopPerformancesViewController
    var isStartUp = true
    
    init(storage: AppStorageManager, apiClient: APIClient, networkStatus: @escaping () -> NWPath.Status) {
        topPerfsVC = TopPerformancesViewController(storage: storage, api: apiClient)
        super.init(rootViewController: topPerfsVC, storage: storage, networkStatus: networkStatus)
        
        topPerfsVC.api = apiClient
        topPerfsVC.onYearTapped = pushPerformancesController
        topPerfsVC.onShowTapped = pushSourcesController
        topPerfsVC.onChangeBandsTapped = presentBandsController
        
        NotificationCenter.default.addObserver(forName: .BandDidChange, object: nil, queue: .main) { notification in
            let band = notification.object as! Band
            self.topPerfsVC.band = band
        }
        
        navigationController.tabBarItem = UITabBarItem(title: "Browse", image: UIImage(icon: .musicNoteList), tag: 1)
        restore()
    }
    
    
    
    func restore() {
        switch storage.browseState {
        case .year(let band, let year):
            pushPerformancesController(band: band, year: year)
        case .performance(let band, let performance):
            pushSourcesController(band: band, show: performance)
        case .recording(let band, let performance, let recording):
            pushSourcesController(band: band, show: performance)
            pushRecordingController(band: band, performance: performance, recording: recording)
        default: break
        }
    }
    
    func navigationController(_ navigationController: UINavigationController, didShow vc: UIViewController, animated: Bool) {
        guard !isStartUp else { isStartUp = false; return }
        if let topShowsVC = vc as? TopPerformancesViewController {
            storage.browseState = .band(topShowsVC.band)
        } else if let yearVC = vc as? PerformancesViewController {
            storage.browseState = .year(yearVC.band, yearVC.year)
        } else if let recsVC = vc as? RecordingsViewController {
            storage.browseState = .performance(recsVC.band, recsVC.performance)
        } else if let recVC = vc as? RecordingViewController {
            storage.browseState = .recording(recVC.viewModel.band, recVC.viewModel.performance, recVC.viewModel.recording)
        }
    }
    
    private var cancellable: AnyCancellable?
    func openToRecording(identifier: String) {
        if let stored = storage.recordings.first(where: { $0.recording.identifier == identifier }) {
            pushRecordingController(stored)
            return
        }
        
        cancellable = apiClient.getRecording(identifier: identifier)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { _ in }, receiveValue: { response in
                let stored = self.storage.addRecording(band: response.band, performance: response.performance, recording: response.recording, songs: response.songs)
                
                self.pushRecordingController(stored)
            })
    }
}
