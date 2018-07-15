//
//  ShowsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class ShowsViewController: UIViewController {
    
    @IBOutlet weak var collectionView: UICollectionView!
    
    var year: Year!
    var shows: [Show] = []
    
    var dataStore: DataStore!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        collectionView.dataSource = self
        collectionView.delegate = self
        
        navigationItem.title = year.year
        
        loadData()
    }
    
    func loadData() {
        dataStore.fetchShows(in: year) { [weak self] result in
            switch result {
            case .success(let shows):
                self?.shows = shows
                DispatchQueue.main.async { self?.collectionView.reloadData() }
            case .failure(let error):
                print(error)
            }
        }
    }
    
}

extension ShowsViewController: UICollectionViewDataSource, UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return shows.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Show Cell", for: indexPath) as? ShowCollectionViewCell else { fatalError() }
        let show = shows[indexPath.item]
        
        cell.dateLabel.text = show.date
        cell.venueLabel.text = show.venue
        cell.locationLabel.text = "\(show.city), \(show.state)"
        cell.sourcesLabel.text = "\(show.sources) sources"
        cell.recordingTypesLabel.text = "SBD"
        cell.stars.rating = show.avgRating
        
        cell.view.roundCorners()
        cell.view.setShadow()
        
        return cell
    }
    
    
}
