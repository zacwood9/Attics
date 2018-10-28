//
//  ShowsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class ShowsViewController: UICollectionViewController, Refreshable {
    var refreshControl = UIRefreshControl()
    
    var year: Year!
    var shows: [Show] = []
    
    var dataStore: DataStore!
    
    var onShowTapped: (Show) -> () = { _ in }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        navigationItem.title = year.year
        extendedLayoutIncludesOpaqueBars = true
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
        
        refresh()
    }
    
    @objc func refresh() {
        dataStore.fetchShows(in: year) { [weak self] result in
            switch result {
            case .success(let shows):
                self?.shows = shows
                DispatchQueue.main.async {
                    self?.collectionView.reloadData()
                    self?.refreshControl.endRefreshing()
//                    let indexPaths = (0..<shows.count).map { IndexPath(item: $0, section: 0) }
//                    self?.collectionView?.insertItems(at: indexPaths)
                }
            case .failure(let error):
                print(error)
            }
        }
    }
    
}

extension ShowsViewController {
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return shows.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Show Cell", for: indexPath) as? ShowCollectionViewCell else { fatalError() }
        let show = shows[indexPath.item]
        
        cell.dateLabel.text = show.date
        cell.dateLabel.font = UIFont.preferredFont(forTextStyle: .title1, withSymbolicTraits: .traitBold)
        cell.venueLabel.text = show.venue
        cell.locationLabel.text = "\(show.city), \(show.state)"
        cell.sourcesLabel.text = "\(show.sources) sources"
        cell.recordingTypesLabel.text = ""
        cell.stars.rating = show.avgRating
        
        cell.view.roundCorners()
        cell.view.setShadow()
        
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onShowTapped(shows[indexPath.item])
    }
}

extension ShowsViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}
