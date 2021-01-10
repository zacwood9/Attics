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

enum RestoreState {
    case band(Band)
    case year(Band, Year)
    case performance(Band, Show)
    case recording(Band, Show, Source)
}

final class BrowseNavigator: AtticsNavigator {
    var state = AppState(id: "browse-v1.3")
    let topPerfsVC: TopPerformancesViewController
    
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
    }
    
    func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated: Bool) {
        let a: [RestoreState] = navigationController.viewControllers.compactMap { vc in
            if let topShowsVC = vc as? TopPerformancesViewController {
                return .band(topShowsVC.band)
            } else if let yearVC = vc as? PerformancesViewController {
                return .year(yearVC.band, yearVC.year)
            } else if let recsVC = vc as? RecordingsViewController {
                return .performance(recsVC.band, recsVC.performance)
            } else if let recVC = vc as? RecordingViewController {
                return .recording(recVC.viewModel.band, recVC.viewModel.performance, recVC.viewModel.recording)
            }
            return nil
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
