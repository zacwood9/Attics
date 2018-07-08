//
//  SourcesViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class SourcesViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    //MARK: Properties
    
    lazy var tableView: UITableView = {
        let tableView = UITableView(frame: self.view.bounds)
        tableView.delegate = self
        tableView.dataSource = self
        tableView.translatesAutoresizingMaskIntoConstraints = false
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: "Source Cell")
        return tableView
    }()
    
    var show: Show
    var sources: [Source] = []
    
    private var dataStore: DataStore
    
    // MARK: Initialization
    
    init(for show: Show, dataStore: DataStore) {
        self.show = show
        self.dataStore = dataStore
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        loadData()
    }
    
    func setupViews() {
        navigationItem.title = String(describing: show.date)
        
        self.view.addSubview(tableView)
        NSLayoutConstraint.activate([tableView.topAnchor.constraint(equalTo: view.topAnchor),
                                     tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor),
                                     tableView.leftAnchor.constraint(equalTo: view.leftAnchor),
                                     tableView.rightAnchor.constraint(equalTo: view.rightAnchor)])
    }
    
    func loadData() {
        dataStore.fetchSources(for: show) { [weak self] result in
            switch result {
            case .success(let sources):
                self?.sources = sources
            case .failure(let error):
                print(error)
            }
            
            DispatchQueue.main.async { [weak self] in
                self?.tableView.reloadData()
            }
        }
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return sources.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Source Cell", for: indexPath)
        cell.textLabel?.text = sources[indexPath.row].transferer
        return cell
    }
    
    // MARK: UITableViewDelegate
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let sourceSelected = sources[indexPath.row]
        let nextVC = configureNextScreen(for: sourceSelected)
        navigationController?.pushViewController(nextVC, animated: true)
    }
    
    func configureNextScreen(for source: Source) -> SongsViewController {
        let vc = SongsViewController(from: source, in: show, dataStore: dataStore)
        return vc
    }

}
