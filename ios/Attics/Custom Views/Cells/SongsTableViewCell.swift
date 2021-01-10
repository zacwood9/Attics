//
//  SongsTableViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/15/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class SongsTableViewCell: UITableViewCell {
    let trackLabel = ATSLabel(textStyle: .footnote, color: .tertiaryLabel)
    let songLabel = ATSLabel(textStyle: .body, color: .label)
    let durationLabel = ATSLabel(textStyle: .callout, color: .secondaryLabel)
    let progressView = ATSProgressCircle(color: .systemOrange)
    let checkmark = UIImageView(image: UIImage(systemName: "checkmark", withConfiguration: UIImage.SymbolConfiguration(pointSize: 24, weight: .bold))!)
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        configureLabels()
        configureProgress()
        configureCheckmark()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    private func configureLabels() {
        contentView.addSubview(trackLabel)
        trackLabel.textAlignment = .right
        
        contentView.addSubview(durationLabel)
        durationLabel.textAlignment = .right
        
        contentView.addSubview(songLabel)
        songLabel.adjustsFontSizeToFitWidth = true
        songLabel.minimumScaleFactor = 0.8
        songLabel.lineBreakMode = .byTruncatingTail
        
        NSLayoutConstraint.activate([
            trackLabel.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 16),
            trackLabel.widthAnchor.constraint(equalToConstant: 18),
            trackLabel.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            
            durationLabel.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -16),
            durationLabel.widthAnchor.constraint(equalToConstant: 64),
            durationLabel.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            
            songLabel.leadingAnchor.constraint(equalTo: trackLabel.trailingAnchor, constant: 16),
            songLabel.trailingAnchor.constraint(equalTo: durationLabel.leadingAnchor, constant: -16),
            songLabel.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
        ])
    }
    
    private func configureProgress() {
        contentView.addSubview(progressView)        
        progressView.isHidden = true
        
        NSLayoutConstraint.activate([
            progressView.trailingAnchor.constraint(equalTo: durationLabel.trailingAnchor, constant: -10),
            progressView.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            progressView.widthAnchor.constraint(equalToConstant: 20),
            progressView.heightAnchor.constraint(equalToConstant: 20),
        ])
    }
    
    private func configureCheckmark() {
        contentView.addSubview(checkmark)
        checkmark.translatesAutoresizingMaskIntoConstraints = false
        checkmark.isHidden = true
        checkmark.tintColor = .systemGreen
        
        NSLayoutConstraint.activate([
            checkmark.trailingAnchor.constraint(equalTo: durationLabel.trailingAnchor, constant: -10),
            checkmark.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
        ])
    }
}
