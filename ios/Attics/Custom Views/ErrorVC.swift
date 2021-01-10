//
//  ErrorVC.swift
//  Attics
//
//  Created by Zachary Wood on 1/8/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit

class ErrorVC: UIViewController {
    
    let stack = UIStackView()
    let imageView = UIImageView(image: UIImage(icon: .wifiSlash, size: 64))
    let label = ATSLabel(textStyle: .body, color: .label)
    let retryButton = UIButton()
    
    var retryFunc: (() -> ())?
    
    init(retry: (() -> ())? = nil) {
        self.retryFunc = retry
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
    
    @objc func refresh() {
        retryFunc?()
    }
    
    private func configureViews() {
        view.addSubview(stack)
        stack.translatesAutoresizingMaskIntoConstraints = false
        stack.axis = .vertical
        stack.spacing = 16
        
        stack.addArrangedSubview(imageView)
        imageView.contentMode = .scaleAspectFit
        stack.addArrangedSubview(label)
        label.text = "Please connect to the internet and try again."
        label.numberOfLines = 0
        label.textAlignment = .center
        
        if retryFunc != nil {
            stack.addArrangedSubview(retryButton)
            retryButton.setTitle("Retry", for: .normal)
            retryButton.setTitleColor(.white, for: .normal)
            retryButton.roundCorners()
            retryButton.layer.borderColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
            retryButton.layer.borderWidth = 4
            retryButton.backgroundColor = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
            retryButton.addTarget(self, action: #selector(refresh), for: .touchUpInside)
            stack.setCustomSpacing(48, after: label)
        }
        
        NSLayoutConstraint.activate([
            stack.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            stack.centerYAnchor.constraint(equalTo: view.centerYAnchor, constant: -100),
            
            label.widthAnchor.constraint(equalToConstant: 200),
        ])
    }
}
