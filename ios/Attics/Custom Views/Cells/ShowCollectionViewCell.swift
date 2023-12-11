//
//  ShowCollectionViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import Cosmos

class ShowCollectionViewCell: UICollectionViewCell {
    let viewStack: UIStackView = {
        let v = UIStackView()
        v.alignment = .leading
        v.axis = .vertical
        v.distribution = .fill
        v.translatesAutoresizingMaskIntoConstraints = false
        return v
    }()
    
    let firstRow: UIStackView = {
        let v = UIStackView()
        v.axis = .horizontal
        v.alignment = .top
        v.distribution = .equalSpacing
        v.translatesAutoresizingMaskIntoConstraints = false
        return v
    }()
    
    let venueLocationStack: UIStackView = {
        let v = UIStackView()
        v.axis = .vertical
        v.alignment = .leading
        v.translatesAutoresizingMaskIntoConstraints = false
        return v
    }()
    
    let starRecordingStack: UIStackView = {
        let v = UIStackView()
        v.axis = .vertical
        v.alignment = .trailing
        v.translatesAutoresizingMaskIntoConstraints = false
        return v
    }()
    
    let dateLabel = ATSLabel(textStyle: .headline, color: .white)
    let venueLabel = ATSLabel(textStyle: .subheadline, color: .lightGray)
    let locationLabel = ATSLabel(textStyle: .subheadline, color: .lightGray)
    let stars: CosmosView = {
        let v = CosmosView()
        v.settings.fillMode = .precise
        v.settings.starSize = 23
        v.settings.starMargin = 0
        v.settings.updateOnTouch = false
        return v
    }()
    let recordingsLabel = ATSLabel(textStyle: .subheadline, color: .lightGray)

    
    override init(frame: CGRect) {
        super.init(frame: frame)
        contentView.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
                
        contentView.addSubview(viewStack)
        viewStack.addArrangedSubview(firstRow)
        
        firstRow.addArrangedSubview(venueLocationStack)
        venueLocationStack.addArrangedSubview(venueLabel)
        venueLocationStack.addArrangedSubview(locationLabel)
        
        firstRow.addArrangedSubview(starRecordingStack)
        starRecordingStack.addArrangedSubview(stars)
        starRecordingStack.addArrangedSubview(recordingsLabel)
        
        viewStack.addArrangedSubview(dateLabel)
        
        NSLayoutConstraint.activate([
            viewStack.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 8),
            viewStack.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -8),
            viewStack.topAnchor.constraint(equalTo: contentView.topAnchor, constant: 8),
            viewStack.bottomAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -8),
            
            firstRow.trailingAnchor.constraint(equalTo: viewStack.trailingAnchor),
        ])
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
}
