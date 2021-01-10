//
//  YearsTableViewCell.swift
//  Attics
//
//  Created by Zachary Wood on 7/14/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

class YearsTableViewCell: UITableViewCell {
    let viewStack = UIStackView() // vertical stack view for the entire cell
    let labelStack = UIStackView() // horizontal stack view for the top labels
    let yearLabel = UILabel()
    let viewDetailLabel = UILabel()
    let seeAllLabel = UILabel()
    let topShowsView: UICollectionView
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        let layout = UICollectionViewFlowLayout()
        layout.itemSize = CGSize(width: 130, height: 130)
        layout.scrollDirection = .horizontal
        layout.sectionInset = UIEdgeInsets(top: 0, left: 8, bottom: 0, right: 8)
        topShowsView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        contentView.backgroundColor = .systemBackground
        
        configureViewStack()
        configureYearStackView()
        configureYearLabel()
        configureSeeAllLabel()
        configureTopShows()
    }
    
    func configureViewStack() {
        contentView.addSubview(viewStack)
        viewStack.translatesAutoresizingMaskIntoConstraints = false
        viewStack.axis = .vertical
        viewStack.addArrangedSubview(labelStack)
        viewStack.addArrangedSubview(topShowsView)
        
        NSLayoutConstraint.activate([
            viewStack.topAnchor.constraint(equalTo: contentView.topAnchor),
            viewStack.leadingAnchor.constraint(equalTo: contentView.leadingAnchor),
            viewStack.trailingAnchor.constraint(equalTo: contentView.trailingAnchor),
            viewStack.bottomAnchor.constraint(equalTo: contentView.bottomAnchor),
        ])
    }
    
    func configureYearStackView() {
        labelStack.addArrangedSubview(yearLabel)
        labelStack.addArrangedSubview(seeAllLabel)
        labelStack.isLayoutMarginsRelativeArrangement = true
        labelStack.layoutMargins = UIEdgeInsets(top: 16, left: 24, bottom: 0, right: 24)
        labelStack.translatesAutoresizingMaskIntoConstraints = false
        labelStack.axis = .horizontal
        labelStack.distribution = .fill
    }
    
    func configureYearLabel() {
        yearLabel.translatesAutoresizingMaskIntoConstraints = false
        yearLabel.font = UIFont.preferredFont(forTextStyle: .title2, withSymbolicTraits: .traitBold)
        yearLabel.textColor = .label
    }
    
    func configureSeeAllLabel() {
        seeAllLabel.text = "See All >"
        seeAllLabel.translatesAutoresizingMaskIntoConstraints = false
        seeAllLabel.font = UIFont.preferredFont(forTextStyle: .footnote)
        seeAllLabel.textColor = .secondaryLabel
    }
    
    func configureTopShows() {
        topShowsView.backgroundColor = .systemBackground
        topShowsView.translatesAutoresizingMaskIntoConstraints = false
        topShowsView.showsHorizontalScrollIndicator = false
        topShowsView.register(TopShowsCollectionViewCell.self, forCellWithReuseIdentifier: CellTypes.topShowCell)
        NSLayoutConstraint.activate([
            topShowsView.heightAnchor.constraint(equalToConstant: 160)
        ])
    }
    
    var collectionViewOffset: CGFloat {
        get {
            return topShowsView.contentOffset.x
        }
        
        set {
            topShowsView.contentOffset.x = newValue
        }
    }
    
    func setCollectionViewDataSourceDelegate
        <D: UICollectionViewDataSource & UICollectionViewDelegate>
        (dataSourceDelegate: D, forRow row: Int) {
        
        topShowsView.delegate = dataSourceDelegate
        topShowsView.dataSource = dataSourceDelegate
        topShowsView.tag = row
        topShowsView.reloadData()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
}
