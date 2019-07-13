//
//  SourcesViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class SourcesViewController: UICollectionViewController, Refreshable {
    var refreshControl = UIRefreshControl()
    
    var show: Show?
    var sources: [Source] = []
    
    var dataStore: SourcesDataStore!
    
    var onSourceTap: (Source) -> () = { _ in }
    
    var configureCell: (Source, SourceCollectionViewCell) -> () = { source, cell in
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
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
    }
    
    func setupViews() {
        // configure navigation
        if show != nil {
            navigationItem.title = "\(show!.date) sources"
        } else {
            navigationItem.title = "My Shows"
        }
        let backItem = UIBarButtonItem(title: "Sources", style: .plain, target: nil, action: nil)
        navigationItem.backBarButtonItem = backItem
        
        extendedLayoutIncludesOpaqueBars = true
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
    }
    
    func refresh() {
        dataStore.fetchSources(for: show) { [weak self] result in
            switch result {
            case .success(let sources):
                self?.sources = sources
                sources.forEach { print($0.source) }
                DispatchQueue.main.async {
                    self?.collectionView.reloadData()
                    self?.refreshControl.endRefreshing()
                }
            case .failure(let error):
                self?.presentAlert(with: error.message)
            }
        }
    }
}

extension SourcesViewController {
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return sources.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Source Cell", for: indexPath) as? SourceCollectionViewCell else { fatalError("Incorrect Cell type") }
        let source = sources[indexPath.item]
        self.configureCell(source, cell)
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onSourceTap(sources[indexPath.item])
    }
}

extension SourcesViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}
