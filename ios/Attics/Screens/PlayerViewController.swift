//
//  PlayerViewController.swift
//  Attics
//
//  Created by Zachary Wood on 12/31/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI

class PlayerViewController : UIViewController {
    let viewModel: RecordingViewModel
    let musicPlayer: MusicPlayer
    
    private lazy var scrollView = UIScrollView()
    private lazy var header = UIHostingController(rootView: PlayerHeader(viewModel: viewModel))
    private lazy var songList = UIHostingController(
        rootView: SongList(
            viewModel: viewModel,
            musicPlayer: musicPlayer,
            songClick: { [weak self] song in
                guard let self = self else { return }
                self.musicPlayer.play(song, self.musicPlayer.state!.playlist)
            }
        )
    )
    
    private lazy var controls = UIHostingController(rootView: MediaControls(player: musicPlayer))
    
    init(viewModel: RecordingViewModel, musicPlayer: MusicPlayer) {
        self.viewModel = viewModel
        self.musicPlayer = musicPlayer
        
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError()
    }
    
    override func viewDidLoad() {
        viewModel.load()
        
        view.backgroundColor = .systemBackground
        
        scrollView.translatesAutoresizingMaskIntoConstraints = false
        header.view.translatesAutoresizingMaskIntoConstraints = false
        songList.view.translatesAutoresizingMaskIntoConstraints = false
        controls.view.translatesAutoresizingMaskIntoConstraints = false
        
        scrollView.bounces = true
        view.addSubview(scrollView)
        
        let imageView = UIImageView(image: UIImage(systemName: "square.and.arrow.up"))
        imageView.translatesAutoresizingMaskIntoConstraints = false
        imageView.tintColor = .white
        imageView.addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(share)))
        imageView.isUserInteractionEnabled = true
        scrollView.addSubview(imageView)
        
        addChild(header)
        scrollView.addSubview(header.view)
        header.didMove(toParent: self)
        
        addChild(songList)
        scrollView.addSubview(songList.view)
        songList.didMove(toParent: self)
        
        add(controls)
                
        NSLayoutConstraint.activate([
            scrollView.topAnchor.constraint(equalTo: view.topAnchor, constant: 24),
            scrollView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            scrollView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            scrollView.bottomAnchor.constraint(equalTo: controls.view.topAnchor),
            
            imageView.topAnchor.constraint(equalTo: scrollView.topAnchor, constant: 16),
            imageView.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -16),
            imageView.widthAnchor.constraint(equalToConstant: 24),
            imageView.heightAnchor.constraint(equalToConstant: 24),
            
            header.view.topAnchor.constraint(equalTo: imageView.bottomAnchor, constant: 12),
            header.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            header.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            
            songList.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            songList.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            songList.view.bottomAnchor.constraint(equalTo: scrollView.bottomAnchor),
            songList.view.topAnchor.constraint(equalTo: header.view.bottomAnchor),
            
            controls.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            controls.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            controls.view.trailingAnchor.constraint(equalTo: view.trailingAnchor)
        ])
    }
    
    @objc func share() {
        guard let stored = viewModel.storage.getStoredRecording(for: viewModel.recording) else { return }
        let item = ShareItem(stored)
        let ac = UIActivityViewController(activityItems: [item], applicationActivities: nil)
        present(ac, animated: true)
    }
}
