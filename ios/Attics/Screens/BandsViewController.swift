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

class BandsViewController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    let viewModel: BandsViewModel
    
    var bandsSubscriber: AnyCancellable?
    
    init(viewModel: BandsViewModel) {
        self.viewModel = viewModel
        super.init(collectionViewLayout: UICollectionViewFlowLayout())
    }
    
    required init?(coder: NSCoder) {
        fatalError()
    }
    
    override func viewDidLoad() {
        BandCollectionViewCell.register(with: collectionView)
        
        bandsSubscriber = viewModel.$bands.receive(on: DispatchQueue.main).sink(receiveValue: { _ in
            print("reloading")
            self.collectionView.reloadData()
        })
        
        viewModel.load()
        
        navigationItem.title = "Bands"
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch viewModel.bands {
        case .success(let bands):
            return bands.count
        default:
            return 1
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = BandCollectionViewCell.getReusedCellFrom(collectionView, cellForItemAt: indexPath)
        switch viewModel.bands {
        case .success(let bands):
            let band = bands[indexPath.item]
            cell.hostingController.rootView = AnyView(BandView(band: band, onClick: viewModel.onBandClick))
        case .loading:
            cell.hostingController.rootView = AnyView(LoadingComponent(retry: nil))
        case .error(let error):
            cell.hostingController.rootView = AnyView(Text(error.localizedDescription))
        }
        return cell
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        return CGSize(width: collectionView.frame.width - 32, height: 150)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, insetForSectionAt section: Int) -> UIEdgeInsets {
        return UIEdgeInsets(top: 16, left: 0, bottom: 16, right: 0)
    }
}

class BandCollectionViewCell : UICollectionViewCell {
    private static let reuseId = "BandCVCell"
    
    static func register(with collectionView: UICollectionView) {
        collectionView.register(BandCollectionViewCell.self, forCellWithReuseIdentifier: reuseId)
    }
    
    static func getReusedCellFrom(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> BandCollectionViewCell {
        return collectionView.dequeueReusableCell(withReuseIdentifier: reuseId, for: indexPath) as! BandCollectionViewCell
    }
    
    var hostingController: UIHostingController<AnyView> = UIHostingController(rootView: AnyView(EmptyView()))
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        contentView.addSubview(self.hostingController.view)
        
        hostingController.view.translatesAutoresizingMaskIntoConstraints = false
        
        hostingController.view.topAnchor.constraint(equalTo: contentView.topAnchor).isActive = true
        hostingController.view.leftAnchor.constraint(equalTo: contentView.leftAnchor).isActive = true
        hostingController.view.rightAnchor.constraint(equalTo: contentView.rightAnchor).isActive = true
        hostingController.view.bottomAnchor.constraint(equalTo: contentView.bottomAnchor).isActive = true
    }
    
    required init?(coder: NSCoder) {
        fatalError("init?(coder: NSCoder) has not been implemented")
    }
}
