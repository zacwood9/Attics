//
//  ShowsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class ShowsViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    //MARK: Properties
    
    lazy var tableView: UITableView = {
        let tableView = UITableView(frame: self.view.bounds)
        tableView.delegate = self
        tableView.dataSource = self
        tableView.translatesAutoresizingMaskIntoConstraints = false
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: "Show Cell")
        return tableView
    }()
    
    var year: Year
    var shows: [Show] = []
    
    private let dataStore: DataStore
    
    // MARK: View Controller Lifecycle
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        loadData()
    }
    
    func setupViews() {
        navigationItem.title = year.year
        
        self.view.addSubview(tableView)
        NSLayoutConstraint.activate([tableView.topAnchor.constraint(equalTo: view.topAnchor),
                                     tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor),
                                     tableView.leftAnchor.constraint(equalTo: view.leftAnchor),
                                     tableView.rightAnchor.constraint(equalTo: view.rightAnchor)])
    }
    
    func loadData() {
        shows = dataStore.fetchShows(in: year)
    }
    
    //MARK: UITableViewDataSource
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Show Cell", for: indexPath)
        cell.textLabel?.text = String(describing: shows[indexPath.row].date)
        cell.detailTextLabel?.text = String(describing: shows[indexPath.row].venue)
        return cell
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return shows.count
    }
    
    // MARK: UITableViewDelegate
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let showSelected = shows[indexPath.row]
        let nextVC = configureNextScreen(for: showSelected)
        navigationController?.pushViewController(nextVC, animated: true)
    }
    
    func configureNextScreen(for show: Show) -> SourcesViewController {
        let vc = SourcesViewController(for: show, dataStore: dataStore)
        return vc
    }
    
    init(withShowsIn year: Year, dataStore: DataStore) {
        self.year = year
        self.dataStore = dataStore
        super.init(nibName: nil, bundle: nil)        
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
}
