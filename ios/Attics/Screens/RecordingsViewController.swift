//
//  SourcesViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Combine

class RecordingsViewController: UICollectionViewController {
    var refreshControl = UIRefreshControl()
    
    var band: Band!
    var performance: Show!
    var recordings: [Source] = []
    var onSourceTap: (Band, Show, Source) -> () = { _,_,_ in }
    
    var api: APIClient!
        
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
    }
    
    var sortFuncs: [String : (Source, Source) -> Bool] = [
        "Downloads": { $0.downloads > $1.downloads },
        "Avg. Rating": { $0.avgRating > $1.avgRating },
        "Num. Reviews": { $0.numReviews > $1.numReviews }
    ]
    
    var sortBy = "Downloads" {
        didSet {
            self.recordings.sort(by: sortFuncs[sortBy]!)
            collectionView.reloadData()
            sortItem.title = "Sort By: \(sortBy)"
        }
    }
    
    var sortItem: UIBarButtonItem!
    private var cancellable: AnyCancellable?
    
    func setupViews() {
        // configure navigation
        navigationItem.title = "\(performance.date) recordings"
        
        let backItem = UIBarButtonItem(title: "Recordings", style: .plain, target: nil, action: nil)
        navigationItem.backBarButtonItem = backItem
        
        sortItem = UIBarButtonItem(title: "Sort By: \(sortBy)", style: .plain, target: self, action: #selector(presentSortByAlert))
        navigationItem.rightBarButtonItem = sortItem
        
        extendedLayoutIncludesOpaqueBars = true
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
    }
    
    @objc func refresh() {
        cancellable = api.getShow(for: band, recordingsOf: performance.date)
            .receive(on: DispatchQueue.main)
            .sink { [weak self] completion in
                switch completion {
                case .failure(let error):
                    self?.presentAlert(with: error.localizedDescription)
                default: return
                }
            } receiveValue: { [weak self] showResponse in
                self?.recordings = showResponse.recordings.sorted(by: self!.sortFuncs[self!.sortBy]!)
                self?.collectionView.reloadData()
                self?.refreshControl.endRefreshing()
            }
    }
    
    @objc func presentSortByAlert() {
        var actions = [UIAlertAction]()
        
        sortFuncs.forEach { key, value in
            let action = UIAlertAction(title: key, style: .default) { _ in
                self.sortBy = key
            }
            actions.append(action)
        }
        
        presentAlert(with: actions)
    }
}

extension RecordingsViewController {
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return recordings.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Source Cell", for: indexPath) as? SourceCollectionViewCell else { fatalError("Incorrect Cell type") }
        let source = recordings[indexPath.item]
        cell.lineageLabel.text = source.lineage
        cell.downloadsLabel.text = "\(source.downloads) downloads"
        cell.reviewsLabel.text = "\(source.numReviews) reviews"
        cell.transfererLabel.text = source.transferer
        cell.transfererLabel.font = UIFont.preferredFont(forTextStyle: .title2, withSymbolicTraits: .traitBold)
        cell.stars.rating = source.avgRating
        
        cell.recordingTypeLabel.text = source.type.rawValue
        cell.recordingTypeLabel.layer.masksToBounds = true
        cell.recordingTypeLabel.layer.cornerRadius = 8.0
        cell.recordingTypeWrapper.roundCorners()
        cell.recordingTypeWrapper.isHidden = source.type == .unknown
        cell.view.roundCorners()
        cell.view.setShadow()
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onSourceTap(band, performance, recordings[indexPath.item])
    }
}

extension RecordingsViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}

struct ShowResponse : Codable {
    let band: Band
    let performance: Show
    let recordings: [Source]
}
