//
//  YearsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Combine
import Cosmos

class TopPerformancesViewController: UIViewController {
    //MARK: Properties
    
    var band: Band {
        didSet {
            navigationItem.title = band.name
            refresh()
        }
    }
    var topPerformances: [(Year, [Show])] = []
    
    var api: APIClient
    
    var onYearTapped: (Band, Year) -> () = { _,_ in }
    var onShowTapped: (Band, Show) -> () = { _,_ in }
    var onChangeBandsTapped: () -> () = {}
    
    private let showsToLoad = UIDevice.current.userInterfaceIdiom == .pad ? 10 : 5
    
    // used to store how far each collection view has been scrolled
    // so it can be restored to that point when dequeued
    private var offsets: [Int:CGFloat] = [:]
    private var cancellable: AnyCancellable?
    
    
    //MARK: Views
    let tableView = UITableView()
    let refreshControl = UIRefreshControl()
    lazy var errorVC: ErrorVC = {
        return ErrorVC(retry: { [weak self] in self?.refresh() })
    }()
    
    var bandCancellable: AnyCancellable?
    
    init(storage: AppStorageManager, api: APIClient) {
        self.band = storage.band
        self.api = api
        super.init(nibName: nil, bundle: nil)
        
        bandCancellable = storage.bandPublisher.receive(on: DispatchQueue.main).sink { [weak self] band in self?.band = band }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(error:) not implemented")
    }
    
    // MARK: View Controller Lifecycle
    
    
    override func viewDidLoad() {
        super.viewDidLoad()
        configureNavigationBar()
        configureTableView()
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
        refresh()
    }
    
    @objc func refresh() {
        errorVC.remove()
        
        cancellable = api.getTopPerformances(band)
            .receive(on: DispatchQueue.main)
            .sink { [weak self] completion in
                switch completion {
                case .failure(let error):
                    print(error)
                    self?.showErrorOnMainThread()
                default: return
                }
            } receiveValue: { [weak self] bandResponse in
                self?.topPerformances = bandResponse.topPerformances.map { $0 }.sorted(by: {$0.0 < $1.0})
                self?.updateTableViewOnMainThread()
                self?.refreshControl.endRefreshing()
            }
    }
    
    @objc func changeBand() {
        onChangeBandsTapped()
    }
    
    private func updateTableViewOnMainThread() {
        self.tableView.isHidden = false
        self.tableView.reloadData()
    }
    
    private func showErrorOnMainThread() {
        DispatchQueue.main.async {
            self.tableView.isHidden = true
            self.add(self.errorVC)            
        }
    }
    
    
    // MARK: - View Configuration
    
    private func configureTableView() {
        view.addSubview(tableView)
        tableView.translatesAutoresizingMaskIntoConstraints = false
        tableView.dataSource = self
        tableView.delegate = self
        tableView.refreshControl = refreshControl
        tableView.register(YearsTableViewCell.self, forCellReuseIdentifier: CellTypes.yearCell)
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 200
        NSLayoutConstraint.activate([
            tableView.topAnchor.constraint(equalTo: view.topAnchor),
            tableView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            tableView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])
    }
    
    func configureNavigationBar() {
        navigationItem.title = band.name
        extendedLayoutIncludesOpaqueBars = true // TODO: Add to other views
        navigationItem.rightBarButtonItem = UIBarButtonItem(title: "Change band", style: .plain, target: self, action: #selector(changeBand))
    }
}

// MARK: - TableView

extension TopPerformancesViewController: UITableViewDelegate, UITableViewDataSource {
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: CellTypes.yearCell, for: indexPath) as! YearsTableViewCell
        cell.yearLabel.text = topPerformances[indexPath.row].0
        return cell
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return topPerformances.count
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
        onYearTapped(band, topPerformances[indexPath.row].0)
    }
}

// MARK: - CollectionView

extension TopPerformancesViewController: UICollectionViewDelegate, UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return topPerformances[collectionView.tag].1.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: CellTypes.topShowCell, for: indexPath) as! TopShowsCollectionViewCell
        let year = topPerformances[collectionView.tag]
        let show = year.1[indexPath.item]
        cell.showLabel.text = show.date
        cell.numSourcesLabel.text = "\(show.venue)"
        cell.cosmosView.rating = show.avgRating
        cell.cosmosView.settings.updateOnTouch = false
        cell.contentView.roundCorners()
        cell.contentView.setShadow()
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onShowTapped(band, topPerformances[collectionView.tag].1[indexPath.item])
    }
}

struct BandResponse : Codable {
    let topPerformances: [Year : [Show]]
    let band: Band
}
