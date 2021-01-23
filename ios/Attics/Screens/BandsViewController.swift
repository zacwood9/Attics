//
//  BandsViewController.swift
//  Attics
//
//  Created by Zachary Wood on 8/15/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI
import Combine
import Network

struct BandsView : View {
    @ObservedObject var viewModel: BandsViewModel
    var searchString: String = ""
    
    var body: some View {
        ScrollView {
            switch viewModel.bands {
            case .success(let bands):
                let displayedBands = searchString.isEmpty
                    ? bands
                    : bands.filter { $0.name.lowercased().contains(searchString) }
                
                VStack(spacing: 12) {
                    ForEach(displayedBands, id: \.collection) { band in
                        BandView(band: band, onClick: viewModel.onBandClick)
                            .padding([.leading, .trailing], 12)
                    }
                }.background(Color(.systemBackground)).padding(.top, 12)
            default: LoadingComponent(retry: nil).onAppear(perform: viewModel.load)
            }
        }
    }
}

class BandsViewController: UICollectionViewController, UICollectionViewDelegateFlowLayout, UISearchResultsUpdating {
    let viewModel: BandsViewModel
    var bandsSubscriber: AnyCancellable?
    
    @Published var searchText: String = ""
    
    private lazy var scrollView = UIScrollView()
    private lazy var hostingController = UIHostingController<BandsView>(rootView: BandsView(viewModel: viewModel))
    
    init(viewModel: BandsViewModel) {
        self.viewModel = viewModel
        super.init(collectionViewLayout: UICollectionViewFlowLayout())
    }
    
    required init?(coder: NSCoder) {
        fatalError()
    }
    
    override func viewDidLoad() {
        navigationItem.title = "Bands"
        navigationController?.navigationBar.prefersLargeTitles = true
        configureViews()
        configureSearch()
    }
    
    private func configureViews() {
        hostingController.view.translatesAutoresizingMaskIntoConstraints = false
        add(hostingController)
        
        NSLayoutConstraint.activate([
            hostingController.view.topAnchor.constraint(equalTo: view.topAnchor),
            hostingController.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            hostingController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            hostingController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor)
        ])
    }
    
    private func configureSearch() {
        let searchController = UISearchController()
        searchController.searchBar.barTintColor = .white
        searchController.searchResultsUpdater = self
        searchController.obscuresBackgroundDuringPresentation = false
        navigationItem.searchController = searchController
    }
    
    func updateSearchResults(for searchController: UISearchController) {
        guard let text = searchController.searchBar.text else { return }
        hostingController.rootView = BandsView(viewModel: viewModel, searchString: text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased())
    }
}
