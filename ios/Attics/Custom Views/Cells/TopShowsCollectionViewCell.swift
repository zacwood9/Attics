//
//  TopShowsCollectionViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Cosmos

class TopShowsCollectionViewCell: UICollectionViewCell {
    let cosmosView = CosmosView()
    let showLabel = UILabel()
    let numSourcesLabel = UILabel()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        contentView.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
        configureCosmos()
        configureLabels()        
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    private func configureCosmos() {
        contentView.addSubview(cosmosView)
        cosmosView.translatesAutoresizingMaskIntoConstraints = false
        cosmosView.settings.fillMode = .precise
        cosmosView.settings.starSize = 23
        cosmosView.settings.starMargin = 0
        cosmosView.rating = 4.3 // TODO: Configure
        NSLayoutConstraint.activate([
            cosmosView.topAnchor.constraint(equalTo: contentView.topAnchor, constant: 8),
            cosmosView.leftAnchor.constraint(equalTo: contentView.leftAnchor, constant: 8),
            cosmosView.heightAnchor.constraint(equalToConstant: 23)
        ])
    }
    
    private func configureLabels() {
        contentView.addSubview(showLabel)
        showLabel.translatesAutoresizingMaskIntoConstraints = false
        showLabel.font = UIFont.preferredFont(forTextStyle: .headline)
        showLabel.textColor = .white
        showLabel.textAlignment = .left
        
        contentView.addSubview(numSourcesLabel)
        numSourcesLabel.translatesAutoresizingMaskIntoConstraints = false
        numSourcesLabel.font = UIFont.preferredFont(forTextStyle: .footnote)
        numSourcesLabel.textColor = .lightGray
        numSourcesLabel.textAlignment = .left
        
        NSLayoutConstraint.activate([
            numSourcesLabel.leftAnchor.constraint(equalTo: contentView.leftAnchor, constant: 8),
            numSourcesLabel.rightAnchor.constraint(equalTo: contentView.rightAnchor, constant: -8),
            numSourcesLabel.bottomAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -8),
            
            showLabel.leftAnchor.constraint(equalTo: contentView.leftAnchor, constant: 8),
            showLabel.rightAnchor.constraint(equalTo: contentView.rightAnchor, constant: -8),
            showLabel.bottomAnchor.constraint(equalTo: numSourcesLabel.topAnchor),
            showLabel.topAnchor.constraint(lessThanOrEqualTo: cosmosView.bottomAnchor, constant: 45)
        ])
    }
}
