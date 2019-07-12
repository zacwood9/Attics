//
//  SourcesViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class MyShowsViewController: UICollectionViewController, Refreshable {
    var refreshControl = UIRefreshControl()
    
    var sources: [Source] = []
    
    var dataStore: SourcesDataStore!
    
    var onSourceTap: (Source) -> () = { _ in }
    
    var configureCell: (Source, SourceCollectionViewCell) -> () = { source, cell in
        cell.lineageLabel.text = "\(source.show.city), \(source.show.state)"
        cell.downloadsLabel.text = "\(source.show.venue)"
        cell.reviewsLabel.text = "Source: \(source.transferer)"
        cell.transfererLabel.text = source.show.date
        cell.transfererLabel.font = UIFont.preferredFont(forTextStyle: .title1, withSymbolicTraits: .traitBold)
        cell.stars.rating = source.avgRating
        cell.recordingTypeLabel.text = ""
        
        cell.view.roundCorners()
        cell.view.setShadow()
    }
    
    let label: UILabel = {
       let label = UILabel()
        label.text = "Tap the favorite button to save your favorite shows here!"
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        label.textAlignment = .center
        return label
    }()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        refresh()
        NotificationCenter.default.addObserver(self, selector: #selector(objcRefresh(_:)), name: .FavoriteWasAdded, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(objcRefresh(_:)), name: .FavoriteWasRemoved, object: nil)
    }
    
    func setupViews() {
        // configure navigation
        navigationItem.title = "My Shows"
        
        extendedLayoutIncludesOpaqueBars = true
        collectionView.refreshControl = refreshControl
        refreshControl.addTarget(self, action: #selector(refresh), for: .valueChanged)
        view.addSubview(label)
        NSLayoutConstraint.activate([
            label.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            label.centerYAnchor.constraint(equalTo: view.centerYAnchor),
            label.leftAnchor.constraint(equalTo: view.leftAnchor, constant: 64),
            label.rightAnchor.constraint(equalTo: view.rightAnchor, constant: -64)
            ])
    }
    
    @objc func objcRefresh(_ notification: NSNotification) { refresh() }
    
    func refresh() {
        dataStore.fetchSources(for: nil) { [weak self] result in
            switch result {
            case .success(let sources):
                self?.sources = sources.sorted { $0.show.date < $1.show.date }
                DispatchQueue.main.async {
                    self?.collectionView.reloadData()
                    self?.refreshControl.endRefreshing()
                    self?.label.isHidden = !sources.isEmpty
                }
            case .failure(let error):
                print(error)
            }
        }
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
}

extension MyShowsViewController {
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return sources.count
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        guard let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "Source Cell", for: indexPath) as? SourceCollectionViewCell else { fatalError("Incorrect Cell type") }
        let source = sources[indexPath.item]
        self.configureCell(source, cell)
        return cell
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        onSourceTap(sources[indexPath.item])
    }
}

extension MyShowsViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let width = collectionView.frame.size.width
        if collectionView.traitCollection.horizontalSizeClass == .regular { // iPad
            return CGSize(width: width/2 - 24, height: 110)
        }
        return CGSize(width: width-24, height: 110)
    }
}
