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

class YearsViewController: UIViewController, Refreshable {
    //MARK: Properties
    
    @IBOutlet weak var tableView: UITableView!
    var refreshControl = UIRefreshControl()
    
    var years: [YearWithTopShows] = []
    
    var dataStore: DataStore!
    var cache: DataStore!
    
    var onYearTapped: (Year) -> () = { _ in }
    var onShowTapped: (Show) -> () = { _ in }
    
    // used to store how far each collection view has been scrolled
    // so it can be restored to that point when dequeued
    var offsets: [Int:CGFloat] = [:]
    
    var context: NSManagedObjectContext!
    
    // MARK: View Controller Lifecycle
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
    }
    
    func setupViews() {
        navigationItem.title = "Attics"
        tableView.dataSource = self
        tableView.delegate = self
        tableView.refreshControl = refreshControl
        extendedLayoutIncludesOpaqueBars = true
        
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
    }
    
    @objc func refresh() {
        //let store: DataStore = cacheStatus == .empty ? dataStore : cache
        
        var numShows = 5
        if UIDevice.current.userInterfaceIdiom == .pad {
            numShows = 10
        }
        
        dataStore.fetchTopShows(numShows: numShows) { [weak self] result in
            switch result {
            case .success(let years):
                self?.years = years
                DispatchQueue.main.async {
                    self?.tableView.reloadData()
                    self?.refreshControl.endRefreshing()
                    //if self?.cacheStatus == .empty {
                      //  self?.updateCache()
                    //}
                }
            case .failure(let error):
                print(error.message)
            }
        }
    }
    
    enum CacheStatus {
        case full
        case empty
    }
    
    var cacheStatus: CacheStatus {
        let fr = YearMO.fetchRequest() as NSFetchRequest<YearMO>
        let yearMOs = try! context.fetch(fr)
        if yearMOs.isEmpty {
            return .empty
        }
        return .full
    }
    
    func updateCache() {
        for pair in years {
            let yearMO = YearMO(pair.year, into: context)
            for show in pair.shows {
                let _ = ShowMO(show, for: yearMO, into: context)
            }
        }
        
        do {
            try context.save()
        } catch(let error) {
            print(error)
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

