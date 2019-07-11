//
//  SongsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 6/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import AVKit
import LNPopupController
import FontAwesome

class SongsViewController: UITableViewController {
    
    var source: Source!
    
    var dataStore: SongsDataStore!
    var songs: [Song] = []
    
    var onSongTapped: (Int, [Song]) -> () = { _,_ in }
    var onMoreInfoTapped: (Source, UIView) -> () = { _,_ in }
    var onFavoriteTapped: (Source) -> () = { _ in }
    var isFavorite: () -> Bool = { return false }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        loadData()

        NotificationCenter.default.addObserver(self, selector: #selector(didReceiveSongPlayed(notification:)), name: .MusicPlayerDidPlay, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        tableView.selectRow(at: nil, animated: false, scrollPosition: .none)
    }
    
    func setupViews() {
        navigationItem.title = String(describing: source.show.date)        
        tableView.delegate = self
        tableView.dataSource = self
        extendedLayoutIncludesOpaqueBars = true
        
        NotificationCenter.default.addObserver(self, selector: #selector(setAsFavorite(notification:)), name: .FavoriteWasAdded, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(removeAsFavorite(notification:)), name: .FavoriteWasRemoved, object: nil)
    }
    
    func loadData() {
        let loadingViewController = LoadingViewController()
        add(loadingViewController)
        
        dataStore.fetchSongs(in: source) { [weak self] result in
            switch result {
            case .success(let songs):
                self?.songs = songs
                DispatchQueue.main.async { [weak self] in
                    let indexPaths = (0..<songs.count).map { IndexPath(row: $0, section: 1) }
                    self?.tableView.insertRows(at: indexPaths, with: .automatic)
                    loadingViewController.remove()
                }
            case .failure(let error):
                print(error)
            }            
        }
    }
    
    @objc func didReceiveSongPlayed(notification: Notification) {
        if let song = notification.object as? Song, let indexOfSong = songs.index(of: song) {
            tableView.selectRow(at: IndexPath(row: indexOfSong, section: 1), animated: true, scrollPosition: .none)
        }
    }
    
    @objc func setAsFavorite(notification: Notification) {
        guard (notification.object as? String) == source.identifier else { return }
        tableView.reloadData()
    }
    
    @objc func removeAsFavorite(notification: Notification) {
        guard (notification.object as? String) == source.identifier else { return }
        tableView.reloadData()
    }
}

extension SongsViewController {
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        if indexPath.section == 1 {
            onSongTapped(indexPath.row, songs)
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case 0:
            return 1
        default:
            return songs.count
        }
    }
    
    @objc func moreInfoTapped(_ sender: UITapGestureRecognizer) {
        guard let view = sender.view else {
            fatalError()
        }
        onMoreInfoTapped(source, view)
    }
    
    @objc func favoriteTapped() {
        onFavoriteTapped(source)
        tableView.reloadData()
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            guard let cell = tableView.dequeueReusableCell(withIdentifier: CellTypes.sourceInfoCell, for: indexPath) as? SourceInfoTableViewCell else { fatalError("Wrong cell type") }
            
            cell.venueLabel.text = source.show.venue
            cell.locationLabel.text = "\(source.show.city), \(source.show.state)"
            cell.infoButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .ellipsisH), ofSize: 26)
            cell.infoButton.setTitleColor(.white, for: .normal)
            cell.infoButton.addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(moreInfoTapped(_:))))
            
            cell.favoriteButton.setTitle(fontAwesome: String.fontAwesomeIcon(name: .heart), ofSize: 26)
            if isFavorite() {
                cell.favoriteButton.setTitleColor(.red, for: .normal)
            } else {
                cell.favoriteButton.setTitleColor(.white, for: .normal)
            }            
            cell.favoriteButton.addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(favoriteTapped)))
        
            return cell
        default:
            guard let cell = tableView.dequeueReusableCell(withIdentifier: CellTypes.songCell, for: indexPath) as? SongsTableViewCell else { fatalError("Wrong cell type") }
            let song = songs[indexPath.row]
            
            cell.songLabel.text = song.title
            cell.trackLabel.text = "\(song.track)"
            cell.durationLabel.text = song.length
            return cell
        }
    }
}

class SongsDataSource: NSObject, UITableViewDataSource {
    var songs: [Song] = []
    
    init(songs: [Song] = []) {
        self.songs = songs
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return songs.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: "Song Cell", for: indexPath) as? SongsTableViewCell else { fatalError("Wrong cell type" ) }
        let song = songs[indexPath.row]
        
        cell.songLabel.text = song.title
        cell.trackLabel.text = "\(song.track)"
        cell.durationLabel.text = song.length
        
        return cell
    }
}
