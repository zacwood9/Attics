//
//  LoadingVC.swift
//  Attics
//
//  Created by Zachary Wood on 1/13/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI

class LoadingVC: UIViewController {

    private lazy var stack = UIStackView()
    private lazy var activityView = UIActivityIndicatorView(style: .medium)
    private lazy var label = ATSLabel(textStyle: .footnote, color: .tertiaryLabel)
    
    var retryFunc: (() -> ())?
    
    init() {
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .systemBackground
        configureViews()
    }
    
    private func configureViews() {
        view.addSubview(stack)
        stack.translatesAutoresizingMaskIntoConstraints = false
        stack.axis = .vertical
        stack.spacing = 16
        
        stack.addArrangedSubview(activityView)
        activityView.hidesWhenStopped = true
        activityView.startAnimating()
        
        stack.addArrangedSubview(label)
        label.textAlignment = .center
        label.text = "Loading..."
        label.numberOfLines = 1

        NSLayoutConstraint.activate([
            stack.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            stack.centerYAnchor.constraint(equalTo: view.centerYAnchor),
            
            label.widthAnchor.constraint(equalToConstant: 200),
        ])
    }
}

struct LoadingComponent : UIViewControllerRepresentable {
    let retry: (() -> ())?
    
    func makeUIViewController(context: Context) -> LoadingVC {
        let vc = LoadingVC()
        vc.retryFunc = retry
        return vc
    }
    
    func updateUIViewController(_ uiViewController: LoadingVC, context: Context) {
    }
}
