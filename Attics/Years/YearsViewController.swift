//
//  YearsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Cosmos
import FontAwesome
import CoreData

class YearsViewController: UIViewController {
    
    //MARK: Properties
    
    @IBOutlet weak var tableView: UITableView!
    var refreshControl = UIRefreshControl()
    
    var years: [YearWithTopShows] = []
    
    var network: NetworkDataStore!
    var cache: CacheDataStore!
    
    var onYearTapped: (Year) -> () = { _ in }
    var onShowTapped: (Show) -> () = { _ in }
    
    // used to store how far each collection view has been scrolled
    // so it can be restored to that point when dequeued
    var offsets: [Int:CGFloat] = [:]
    
    var context: NSManagedObjectContext!
    
    let showsToLoad = UIDevice.current.userInterfaceIdiom == .pad ? 10 : 5
    
    // MARK: View Controller Lifecycle
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
//        if cache.status == .full {
//            refresh(from: cache)
//        } else {
            refresh(from: network)
//        }
    }
    
    func setupViews() {
        navigationItem.title = "Attics"
        tableView.dataSource = self
        tableView.delegate = self
        tableView.refreshControl = refreshControl
        extendedLayoutIncludesOpaqueBars = true
        
        refreshControl.addTarget(self, action: #selector(refreshTarget), for: .valueChanged)
    }
    
    @objc func refreshTarget() {
        refresh(from: network)
    }
    
    func refresh(from cache: CacheDataStore) {
        cache.fetchTopShows(numShows: showsToLoad) { [weak self] result in
            switch result {
            case .success(let years):
                self?.years = years
            case .failure(let error):
                self?.presentAlert(with: error.message)
            }
            
            DispatchQueue.main.async {
                self?.tableView.reloadData()
                self?.refreshControl.endRefreshing()
            }
        }
    }
    
    func refresh(from network: NetworkDataStore) {
        network.fetchTopShows(numShows: showsToLoad) { [weak self] result in
            switch result {
            case .success(let years):
                self?.years = years
//                self?.cache.updateCache(years: years)
            case .failure(let error):
                print(error.message)
                self?.presentAlert(with: error.message)
            }
            
            DispatchQueue.main.async {
                self?.tableView.reloadData()
                self?.refreshControl.endRefreshing()
            }
        }
    }
}

extension YearsViewController: UITableViewDelegate, UITableViewDataSource {
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Year Cell", for: indexPath) as! YearsTableViewCell
        cell.yearLabel.text = years[indexPath.row].year.year
        cell.yearLabel.font = UIFont.preferredFont(forTextStyle: .title2, withSymbolicTraits: .traitBold)
        return cell
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return years.count
    }
    
    func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? YearsTableViewCell else { fatalError("Cells must be correct type") }
        
        cell.setCollectionViewDataSourceDelegate(dataSourceDelegate: self, forRow: indexPath.row)
        cell.collectionViewOffset = offsets[indexPath.row] ?? 0
    }
    
    func tableView(_ tableView: UITableView, didEndDisplaying cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? YearsTableViewCell else { fatalError("Cells must be correct type") }
        
        offsets[indexPath.row] = cell.collectionViewOffset
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        onYearTapped(years[indexPath.row].year)
    }
    
}

extension YearsViewController: UICollectionViewDelegate, UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return years[collectionView.tag].shows.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Top Show Cell", for: indexPath) as! TopShowsCollectionViewCell
        let year = years[collectionView.tag]
        let show = year.shows[indexPath.item]
        cell.showLabel.text = show.date
        cell.numSourcesLabel.text = "\(show.venue)"
        cell.stars.settings.fillMode = .precise
        cell.stars.rating = show.avgRating
        cell.roundCorners()
        cell.setShadow()
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onShowTapped(years[collectionView.tag].shows[indexPath.item])
    }
}

