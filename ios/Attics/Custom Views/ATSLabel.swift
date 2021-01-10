//
//  ATSLabel.swift
//  Attics
//
//  Created by Zachary Wood on 1/3/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit

class ATSLabel: UILabel {
    
    init(textStyle: UIFont.TextStyle, color: UIColor) {
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        font = UIFont.preferredFont(forTextStyle: textStyle)
        textColor = color
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }

}
