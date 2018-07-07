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

class SongsViewController: UIViewController, UITableViewDelegate {
    
    lazy var tableView: UITableView = {
        let tableView = UITableView(frame: self.view.bounds)
        tableView.delegate = self
        tableView.dataSource = dataSource
        tableView.translatesAutoresizingMaskIntoConstraints = false
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: "Song Cell")
        return tableView
    }()
    
    var source: Source
    
    private var dataStore: DataStore
    private var dataSource: SongsDataSource
    
    init(from source: Source, dataStore: DataStore) {
        self.source = source
        self.dataStore = dataStore
        self.dataSource = SongsDataSource(songs: [])
        super.init(nibName: nil, bundle: nil)
        
        NotificationCenter.default.addObserver(self, selector: #selector(didReceiveSongPlayed(notification:)), name: .MusicPlayerDidPlay, object: nil)
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupViews()
        loadData()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        tableView.selectRow(at: nil, animated: false, scrollPosition: .none)
    }
    
    func setupViews() {
        navigationItem.title = String(describing: source.identifier)
        
        self.view.addSubview(tableView)
        NSLayoutConstraint.activate([tableView.topAnchor.constraint(equalTo: view.topAnchor),
                                     tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor),
                                     tableView.leftAnchor.constraint(equalTo: view.leftAnchor),
                                     tableView.rightAnchor.constraint(equalTo: view.rightAnchor)])
    }
    
    func loadData() {
//        dataSource.songs = dataStore.fetchSongs(for: source)
    }
    
    @objc func didReceiveSongPlayed(notification: Notification) {
        if let song = notification.object as? Song, let indexOfSong = dataSource.songs.index(of: song) {
            tableView.selectRow(at: IndexPath(row: indexOfSong, section: 0), animated: true, scrollPosition: .none)
        }
    }
    
    var player = AVPlayer(playerItem: nil)
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
//        MusicPlayer.instance.play(index: indexPath.row, in: dataStore.fetchSongs(for: source))
        
        let popupVC = NowPlayingController(selectedIndexPath: indexPath, selectRowCallback: { [weak self] indexPath in
            self?.tableView.selectRow(at: indexPath, animated: false, scrollPosition: .top)
        })
        
        tabBarController?.presentPopupBar(withContentViewController: popupVC, animated: true, completion: nil)                
    }

}

class SongsDataSource: NSObject, UITableViewDataSource {
    var songs: [Song] = []
    
    init(songs: [Song]) {
        self.songs = songs
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return songs.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Song Cell", for: indexPath)
        cell.textLabel?.text = songs[indexPath.row].title
        return cell
    }
}
