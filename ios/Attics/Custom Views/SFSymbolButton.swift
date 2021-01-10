//
//  SFSymbolButton.swift
//  Attics
//
//  Created by Zachary Wood on 1/9/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit

class SFSymbolButton: UIButton {

    init(symbol: SFSymbol, size: CGFloat, color: UIColor? = nil) {
        super.init(frame: .zero)
        
        setImage(UIImage(icon: symbol, size: size), for: .normal)
        if let color = color {
            tintColor = color
        }
        
        translatesAutoresizingMaskIntoConstraints = false
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
}
