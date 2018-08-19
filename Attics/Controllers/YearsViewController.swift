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

class YearsViewController: UIViewController {
    
    //MARK: Properties
    
    @IBOutlet weak var tableView: UITableView!
    
    var years: [YearWithTopShows] = []
    
    var dataStore: DataStore!
    var onYearTapped: (Year) -> () = { _ in }
    var onShowTapped: (Show) -> () = { _ in }
    
    // used to store how far each collection view has been scrolled
    // so it can be restored to that point when dequeued
    var offsets: [Int:CGFloat] = [:]
    
    // MARK: View Controller Lifecycle
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        loadData()
    }
    
    func setupViews() {
        navigationItem.title = "Attics"
        tableView.dataSource = self
        tableView.delegate = self
        extendedLayoutIncludesOpaqueBars = true
    }
    
    func loadData() {
        dataStore.fetchTopShows { [weak self] result in
            switch result {
            case .success(let years):
                self?.years = years
                DispatchQueue.main.async {
//                    self?.tableView.reloadData()
                    let indexPaths = (0..<years.count).map { IndexPath(row: $0, section: 0) }
                    self?.tableView.insertRows(at: indexPaths, with: .automatic)
                }
            case .failure(let error):
                print(error.message)
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

