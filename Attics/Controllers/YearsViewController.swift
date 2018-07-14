//
//  YearsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Cosmos

class YearsViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    //MARK: Properties
    
    @IBOutlet weak var tableView: UITableView!
    
    var years: [NetworkYear] = []
    var dataStore: DataStore!
    
    var didSelectYear: (NetworkYear) -> () = { _ in }
    var didSelectShow: (NetworkShow) -> () = { _ in }
    
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
    }
    
    func loadData() {
        dataStore.fetchYears { [weak self] result in
            switch result {
            case .success(let years):
                self?.years = years
                DispatchQueue.main.async { self?.tableView.reloadData() }
            case .failure(let error):
                print(error.message)
            }
        }
    }
    
    //MARK: UITableViewDataSource
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Year Cell", for: indexPath) as! YearsTableViewCell
        cell.configure(with: years[indexPath.row])
        return cell
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return years.count
    }
    
    // MARK: UITableViewDelegate
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
//        let yearSelected = years[indexPath.row]
//        let nextVC = configureNextScreen(for: yearSelected)
//        navigationController?.pushViewController(nextVC, animated: true)
    }
    
    func configureNextScreen(for year: Year) -> ShowsViewController {
        let vc = ShowsViewController(withShowsIn: year, dataStore: dataStore)
        return vc
    }
}

class YearsTableViewCell: UITableViewCell, UICollectionViewDataSource {
    @IBOutlet weak var yearLabel: UILabel!
    @IBOutlet weak var topShowsView: UICollectionView!
    
    private var topShows: [NetworkShow] = []
    
    func configure(with year: NetworkYear) {
        topShows = year.topShows
        yearLabel.text = year.year
        topShowsView.dataSource = self
        topShowsView.reloadData()
        yearLabel.font = UIFont.preferredFont(forTextStyle: .title1, withSymbolicTraits: .traitBold)
    }
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return topShows.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Top Show Cell", for: indexPath) as! TopShowCollectionViewCell
        let show = topShows[indexPath.item]
        cell.showLabel.text = show.date
        cell.numSourcesLabel.text = "\(show.sources) sources"
        cell.stars.settings.fillMode = .precise
        cell.stars.rating = show.avgRating
        cell.roundCorners()
        cell.setShadow()
        return cell
    }
}

class TopShowCollectionViewCell: UICollectionViewCell {
    @IBOutlet weak var stars: CosmosView!
    @IBOutlet weak var showLabel: UILabel!
    @IBOutlet weak var numSourcesLabel: UILabel!
    
}

extension UIView {
    func setShadow() {
        layer.shadowColor = UIColor.lightGray.cgColor
        layer.shadowOffset = CGSize(width: 0, height: 0)
        layer.shadowRadius = 2.0
        layer.shadowOpacity = 1.0
        layer.masksToBounds = false;
        layer.shadowPath = UIBezierPath(roundedRect: bounds, cornerRadius: layer.cornerRadius).cgPath
        
    }
    
    func roundCorners() {
        layer.cornerRadius = 8.0
        layer.borderWidth = 1.0
        layer.borderColor = UIColor.clear.cgColor
        layer.masksToBounds = false;
    }
}
