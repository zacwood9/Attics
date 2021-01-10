//
//  ShowsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Combine

class PerformancesViewController: UICollectionViewController {
    var refreshControl = UIRefreshControl()
    var band: Band!
    var year: Year!
    var shows: [Show] = [] {
        didSet { showsToDisplay = shows }
    }
    
    var showsToDisplay: [Show] = [] {
        didSet { collectionView.reloadData() }
    }

    var api: APIClient!
    var onShowTapped: (Band, Show) -> () = { _,_ in }
    
    var sortFuncs: [String : (Show, Show) -> Bool] = [
        "Date": { $0.date < $1.date },
        "Rating": {
            $0.avgRating * Double($0.numReviews) > $1.avgRating * Double($1.numReviews)
        }
    ]
    
    var sortBy = "Date" {
        didSet {
            self.shows.sort(by: sortFuncs[sortBy]!)
            collectionView.reloadData()
            sortItem.title = "Sort By: \(sortBy)"
        }
    }
    
    var sortItem: UIBarButtonItem!
    
    private var cancellable: AnyCancellable?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        navigationItem.title = year
        extendedLayoutIncludesOpaqueBars = true
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
        
        sortItem = UIBarButtonItem(title: "Sort By: \(sortBy)", style: .plain, target: self, action: #selector(presentSortByAlert))
        navigationItem.rightBarButtonItem = sortItem
        
        refresh()
        
        let searchController = UISearchController()
        searchController.searchBar.barTintColor = .white
        searchController.searchResultsUpdater = self
        searchController.obscuresBackgroundDuringPresentation = false
        navigationItem.searchController = searchController
    }
    
    @objc func refresh() {
        cancellable = api.getPerformances(for: band, in: year)
            .receive(on: DispatchQueue.main)
            .sink { [weak self] completion in
                switch completion {
                case .failure(let error):
                    self?.presentAlert(with: error.localizedDescription)
                default: return
                }
            } receiveValue: { [weak self] yearResponse in
                self?.shows = yearResponse.performances.sorted(by: self!.sortFuncs[self!.sortBy]!)
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

extension PerformancesViewController {
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return showsToDisplay.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Show Cell", for: indexPath) as? ShowCollectionViewCell else { fatalError() }
        let show = showsToDisplay[indexPath.item]
        
        cell.dateLabel.text = show.date
        cell.dateLabel.font = UIFont.preferredFont(forTextStyle: .title1, withSymbolicTraits: .traitBold)
        cell.venueLabel.text = show.venue
        cell.locationLabel.text = "\(show.city), \(show.state)"
        cell.sourcesLabel.text = "\(show.numRecordings) recordings"
        cell.recordingTypesLabel.text = ""
        cell.stars.rating = show.avgRating
        
        cell.view.roundCorners()
        cell.view.setShadow()
        
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onShowTapped(band, showsToDisplay[indexPath.item])
    }
}

extension PerformancesViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}

extension PerformancesViewController : UISearchResultsUpdating {
    func updateSearchResults(for searchController: UISearchController) {
        guard let text = searchController.searchBar.text else { return }
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        guard trimmed != "" else {
            showsToDisplay = shows.sorted(by: sortFuncs[sortBy]!)
            return
        }
        
        let venueMatches: (Show) -> Bool = { $0.venue.lowercased().contains(trimmed) }
        let cityMatches: (Show) -> Bool = { $0.city.lowercased().starts(with: trimmed) }
        let stateMatches: (Show) -> Bool = { $0.state.lowercased().starts(with: trimmed) }
        let dateMatches: (Show) -> Bool = { $0.date.contains(trimmed) }
        
        let filtered = shows.filter(matchesAny: [venueMatches, cityMatches, stateMatches, dateMatches])
        showsToDisplay = filtered.sorted(by: sortFuncs[sortBy]!)
    }
}

struct YearResponse : Codable {
    let band: Band
    let performances: [Show]
}

extension Array where Element : Equatable {
    func filter(matchesAny: [(Element) -> Bool]) -> [Element] {
        return filter {
            for f in matchesAny {
                if f($0) {
                    return true
                }
            }
            return false
        }
    }
}
