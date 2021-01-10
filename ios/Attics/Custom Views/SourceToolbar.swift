//
//  SourceToolbar.swift
//  Attics
//
//  Created by Zachary Wood on 1/3/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit

protocol ToolbarDelegate: class {
    func downloadPressed()
    func favoritedPressed()
    func moreInfoPressed()
}

class SourceToolbar: UITableViewCell {
    
    struct DownloadProgress {
        let numDownloaded: Int
        let total: Int
    }
    
    enum DownloadState {
        case notDownloaded
        case downloading(DownloadProgress)
        case downloaded
    }
    
    weak var delegate: ToolbarDelegate?
    
    private let containerView = UIView(frame: .zero)
    private let infoStack = UIStackView()
    private let buttonStack = UIStackView()
    let downloadButton = UIButton()
    let favoriteButton = UIButton()
    let moreInfoButton = UIButton()
    
    let downloadStack = UIStackView()
    let downloadProgress = ATSProgressCircle(color: .systemOrange)
    let progressLabel = ATSLabel(textStyle: .footnote, color: .white)
    
    let venueLabel = ATSLabel(textStyle: .body, color: .white)
    let locationLabel = ATSLabel(textStyle: .body, color: .white)
    
    convenience init() {
        self.init(style: .default, reuseIdentifier: nil)
    }
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
        contentView.backgroundColor = .clear
        selectionStyle = .none
        configureContainer()
        configureInfoStack()
        configureButtonStack()
        configureDownloadStack()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    func configure(for state: DownloadState, isFavorite: Bool) {
        switch state {
        case .notDownloaded:
            displayDownloadButton(downloaded: false)
        case .downloaded:
            displayDownloadButton(downloaded: true)
        case .downloading(let progress):
            configureDownloadProgress(downloaded: progress.numDownloaded, total: progress.total)
        }
        
        let icon: SFSymbol = isFavorite ? .heartFill : .heart
        let color: UIColor = isFavorite ? .red : .white
        
        favoriteButton.setImage(UIImage(icon: icon), for: .normal)
        favoriteButton.tintColor = color
    }
    
    private func displayDownloadButton(downloaded: Bool) {
        if buttonStack.arrangedSubviews.first == downloadStack { // wrong view in spot
            buttonStack.removeArrangedSubview(downloadStack)
            downloadStack.isHidden = true
            buttonStack.insertArrangedSubview(downloadButton, at: 0)
            downloadButton.isHidden = false
        }
        
        let imageName: SFSymbol = downloaded ? .downloaded : .download
        let color: UIColor = downloaded ? .green : .white
        
        downloadButton.setImage(UIImage(icon: imageName), for: .normal)
        downloadButton.tintColor = color
    }
    
    
    private func configureDownloadProgress(downloaded: Int, total: Int) {
        if buttonStack.arrangedSubviews.first == downloadButton { // wrong view in spot
            buttonStack.removeArrangedSubview(downloadButton)
            downloadButton.isHidden = true
            buttonStack.insertArrangedSubview(downloadStack, at: 0)
            downloadStack.isHidden = false
        }
        
        downloadProgress.percentageComplete = Double(downloaded)/Double(total)
        progressLabel.text = "\(downloaded)/\(total)"
    }
    
    private func configureContainer() {
        contentView.addSubview(containerView)
        containerView.translatesAutoresizingMaskIntoConstraints = false
        containerView.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
        containerView.roundCorners()
        containerView.setShadow()
        
        NSLayoutConstraint.activate([
            containerView.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 16),
            containerView.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -16),
            containerView.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            containerView.heightAnchor.constraint(equalToConstant: 144),
        ])
    }

    private func configureInfoStack() {
        containerView.addSubview(infoStack)
        infoStack.translatesAutoresizingMaskIntoConstraints = false
        infoStack.axis = .vertical
        infoStack.backgroundColor = .blue
        
        infoStack.addArrangedSubview(venueLabel)
        venueLabel.textAlignment = .center
        
        infoStack.addArrangedSubview(locationLabel)
        locationLabel.textAlignment = .center
        
        NSLayoutConstraint.activate([
            infoStack.centerXAnchor.constraint(equalTo: containerView.centerXAnchor),
            infoStack.centerYAnchor.constraint(equalTo: containerView.centerYAnchor),
            infoStack.leadingAnchor.constraint(equalTo: containerView.leadingAnchor),
            infoStack.trailingAnchor.constraint(equalTo: containerView.trailingAnchor),
        ])
    }
    
    private func configureButtonStack() {
        infoStack.addArrangedSubview(buttonStack)
        buttonStack.isLayoutMarginsRelativeArrangement = true
        buttonStack.layoutMargins = UIEdgeInsets(top: 16, left: 0, bottom: 0, right: 0)
        buttonStack.axis = .horizontal
        buttonStack.distribution = .fillEqually
        
        buttonStack.addArrangedSubview(downloadButton)
        downloadButton.tintColor = .white
        downloadButton.setImage(UIImage(systemName: "square.and.arrow.down", withConfiguration: UIImage.SymbolConfiguration(pointSize: 32, weight: .bold)), for: .normal)
        downloadButton.addTarget(self, action: #selector(downloadTouchTarget), for: .touchUpInside)
        
        buttonStack.addArrangedSubview(favoriteButton)
        favoriteButton.tintColor = .white
        favoriteButton.setImage(UIImage(systemName: "heart", withConfiguration: UIImage.SymbolConfiguration(pointSize: 32, weight: .bold)), for: .normal)
        favoriteButton.addTarget(self, action: #selector(favoriteTouchTarget), for: .touchUpInside)
        
        buttonStack.addArrangedSubview(moreInfoButton)
        moreInfoButton.tintColor = .white
        moreInfoButton.setImage(UIImage(systemName: "ellipsis", withConfiguration: UIImage.SymbolConfiguration(pointSize: 32, weight: .bold)), for: .normal)
        moreInfoButton.addTarget(self, action: #selector(infoTouchTarget), for: .touchUpInside)
    }
    
    private func configureDownloadStack() {
        downloadStack.axis = .vertical
        downloadStack.addArrangedSubview(downloadProgress)
        downloadStack.addArrangedSubview(progressLabel)
        progressLabel.textAlignment = .center
        
        let tap = UITapGestureRecognizer(target: self, action: #selector(downloadTouchTarget))
        downloadStack.addGestureRecognizer(tap)
    }
    
    @objc func downloadTouchTarget() {
        delegate?.downloadPressed()
    }
    
    @objc func favoriteTouchTarget() {
        delegate?.favoritedPressed()
    }
    
    @objc func infoTouchTarget() {
        delegate?.moreInfoPressed()
    }
}
