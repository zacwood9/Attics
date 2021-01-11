//
//  SourcesViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Combine

class MyShowsViewController: UICollectionViewController, UISearchResultsUpdating {
    func updateSearchResults(for searchController: UISearchController) {
        guard let text = searchController.searchBar.text else { return }
        searchText = text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
    }
    
    var searchText = "" { didSet { refresh() }}
    
    var refreshControl = UIRefreshControl()
    
    var recordings: [StoredRecording] = [] {
        didSet { refresh() }
    }
    
    func displayedRecordings() -> [StoredRecording] {
        let venueMatches: (StoredRecording) -> Bool = { $0.performance.venue.lowercased().contains(self.searchText) }
        let cityMatches: (StoredRecording) -> Bool = { $0.performance.city.lowercased().starts(with: self.searchText) }
        let stateMatches: (StoredRecording) -> Bool = { $0.performance.state.lowercased().starts(with: self.searchText) }
        let dateMatches: (StoredRecording) -> Bool = { $0.performance.date.contains(self.searchText) }
        
        return recordings
            .filter(matchesAny: [venueMatches, cityMatches, stateMatches, dateMatches])
            .sorted(by: { $0.performance.date < $1.performance.date })
    }
    
    var storage: AppStorageManager!
    var onSourceTap: (StoredRecording) -> () = { _ in }
    var onChangeBandTap: () -> () = { }
    
    var bandCancellable: AnyCancellable?
    var recordingsCancellable: AnyCancellable?
    
    let label: UILabel = {
        let label = UILabel()
        label.text = "Tap the favorite button to save your favorite shows here!"
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        label.textAlignment = .center
        return label
    }()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
        NotificationCenter.default.addObserver(self, selector: #selector(refresh), name: .FavoritesChanged, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(refresh), name: .BandDidChange, object: nil)
        
        bandCancellable = storage.bandPublisher.receive(on: DispatchQueue.main).sink { band in            
            self.recordings = self.storage.recordings
                .filter { $0.band.collection == band.collection }
                .filter { $0.favorite || $0.downloadState == .downloaded }
        }
        
        recordingsCancellable = storage.recordingsPublisher
            .map { $0.filter { stored in stored.band.collection == self.storage.band.collection }}
            .map { $0.filter { stored in stored.favorite || stored.downloadState == .downloaded }}
            .assign(to: \.recordings, on: self)
        
        let searchController = UISearchController()
        searchController.searchBar.barTintColor = .white
        searchController.searchResultsUpdater = self
        searchController.obscuresBackgroundDuringPresentation = false
        navigationItem.searchController = searchController
    }
    
    
    @objc func changeBand() {
        onChangeBandTap()
    }

    func setupViews() {
        // configure navigation
        navigationItem.title = "My Shows"
        navigationItem.rightBarButtonItem = UIBarButtonItem(title: "Change band", style: .plain, target: self, action: #selector(changeBand))
        navigationItem.leftBarButtonItem = UIBarButtonItem(title: storage.band.name, style: .plain, target: self, action: #selector(changeBand))
        
        extendedLayoutIncludesOpaqueBars = true
        
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
        
        collectionView.addSubview(label)
        NSLayoutConstraint.activate([
            label.centerXAnchor.constraint(equalTo: collectionView.centerXAnchor),
            label.centerYAnchor.constraint(equalTo: collectionView.topAnchor, constant: 64),
            label.leftAnchor.constraint(equalTo: collectionView.leftAnchor, constant: 64),
            label.rightAnchor.constraint(equalTo: collectionView.rightAnchor, constant: -64)
            ])
    }

    @objc func refresh() {
        collectionView.reloadData()
        refreshControl.endRefreshing()
        label.isHidden = !recordings.isEmpty
        
        navigationItem.leftBarButtonItem?.title = storage.band.name
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
}


// MARK: Collection View Delegate Methods
extension MyShowsViewController {
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return displayedRecordings().count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Source Cell", for: indexPath) as? SourceCollectionViewCell else { fatalError("Incorrect Cell type") }
        let stored = displayedRecordings()[indexPath.item]
        let recording = stored.recording
        
        cell.lineageLabel.text = "\(stored.performance.city), \(stored.performance.state)"
        cell.downloadsLabel.text = "\(stored.performance.venue)"
        cell.reviewsLabel.text = "Source: \(recording.transferer)"
        cell.transfererLabel.text = stored.performance.date
        cell.transfererLabel.font = UIFont.preferredFont(forTextStyle: .title1, withSymbolicTraits: .traitBold)
        cell.stars.rating = recording.avgRating
        
        cell.recordingTypeLabel.text = recording.type.rawValue
        cell.recordingTypeLabel.layer.masksToBounds = true
        cell.recordingTypeLabel.layer.cornerRadius = 8.0
        cell.recordingTypeWrapper.roundCorners()
        cell.recordingTypeWrapper.isHidden = stored.recording.type == .unknown
        
        cell.view.roundCorners()
        cell.view.setShadow()
        
        switch stored.downloadState {
        case .downloaded: cell.view.backgroundColor = #colorLiteral(red: 0, green: 0.3046096265, blue: 0.1889052391, alpha: 1)
        default: cell.view.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
        }
        
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onSourceTap(displayedRecordings()[indexPath.row])
    }
}

extension MyShowsViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}
