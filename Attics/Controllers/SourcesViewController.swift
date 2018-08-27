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
    
    var show: Show!
    var sources: [Source] = []
    
    var dataStore: DataStore!
    
    var onSourceTap: (Source) -> () = { _ in }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
    }
    
    func setupViews() {
        navigationItem.title = "\(show.date) sources"
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
                DispatchQueue.main.async {
                    self?.collectionView.reloadData()
                    self?.refreshControl.endRefreshing()
//                    let indexPaths = (0..<sources.count).map { IndexPath(item: $0, section: 0) }
//                    self?.collectionView.insertItems(at: indexPaths)
                }
            case .failure(let error):
                print(error)
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
        
        cell.lineageLabel.text = source.lineage
        cell.downloadsLabel.text = "\(source.downloads) downloads"
        cell.reviewsLabel.text = "\(source.numReviews) reviews"
        cell.transfererLabel.text = source.transferer
        cell.transfererLabel.font = UIFont.preferredFont(forTextStyle: .title2, withSymbolicTraits: .traitBold)
        cell.stars.rating = source.avgRating
        cell.recordingTypeLabel.text = "SBD"
        
        cell.view.roundCorners()
        cell.view.setShadow()
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
