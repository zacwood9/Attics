//
//  UIFont+Extensions.swift
//  Attics
//
//  Created by Zachary Wood on 6/28/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit

extension UIFont {
    class func preferredFont(forTextStyle style: UIFont.TextStyle, withSymbolicTraits traits: UIFontDescriptor.SymbolicTraits) -> UIFont {
        guard let fontDescriptor = UIFont.preferredFont(forTextStyle: style).fontDescriptor.withSymbolicTraits(traits) else { fatalError("Failed to create font") }
        return UIFont(descriptor: fontDescriptor, size: 0)
    }
}
