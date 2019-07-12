//
//  UIFont+Extensions.swift
//  Attics
//
//  Created by Zachary Wood on 6/28/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
import FontAwesome
import SafariServices
import AVFoundation

extension UIFont {
    class func preferredFont(forTextStyle style: UIFont.TextStyle, withSymbolicTraits traits: UIFontDescriptor.SymbolicTraits) -> UIFont {
        guard let fontDescriptor = UIFont.preferredFont(forTextStyle: style).fontDescriptor.withSymbolicTraits(traits) else { fatalError("Failed to create font") }
        return UIFont(descriptor: fontDescriptor, size: 0)
    }
}

extension Double {
    var timeString: String {
        let roundedDown = Int(self)
        let minutes = roundedDown / 60
        let seconds = roundedDown % 60
        var secondsString: String
        if seconds / 10 == 0 {
            secondsString = "0\(seconds)"
        } else {
            secondsString = "\(seconds)"
        }
        return "\(minutes):\(secondsString)"
    }
}

extension UIView {
    func setShadow() {
        layer.shadowColor = UIColor.lightGray.cgColor
        layer.shadowOffset = CGSize(width: 0, height: 0)
        layer.shadowRadius = 2.0
        layer.shadowOpacity = 1.0
        layer.masksToBounds = false;
        layer.shadowPath = UIBezierPath(roundedRect: bounds, cornerRadius: layer.cornerRadius).cgPath
        
    }
    
    func roundCorners() {
        layer.cornerRadius = 8.0
        layer.borderWidth = 1.0
        layer.borderColor = UIColor.clear.cgColor
        layer.masksToBounds = false;
    }
}

extension UIButton {
    func setTitle(fontAwesome icon: String, ofSize size: CGFloat) {
        titleLabel?.font = UIFont.fontAwesome(ofSize: size, style: .solid)
        setTitle(icon, for: .normal)
    }
}

extension UIViewController {
    func presentSafariViewController(at url: URL) {
        let safari = SFSafariViewController(url: url)
        present(safari, animated: true, completion: nil)
    }
    
    func presentAlert(with message: String) {
        let alertVC = UIAlertController(title: "An error has occured.", message: message + "\n Please try again later.", preferredStyle: .alert)
        DispatchQueue.main.async { [weak self] in
            self?.present(alertVC, animated: true, completion: nil)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.5) {
            alertVC.dismiss(animated: true, completion: nil)
        }
    }
}

extension UIViewController {
    func add(_ child: UIViewController) {
        addChild(child)
        view.addSubview(child.view)
        child.didMove(toParent: self)
    }
    
    func remove() {
        // Just to be safe, we check that this view controller
        // is actually added to a parent before removing it.
        guard parent != nil else {
            return
        }
        
        willMove(toParent: nil)
        view.removeFromSuperview()
        removeFromParent()
    }
}

extension UIDevice {
    enum VibrateStyle {
        case light, medium, heavy, select
    }
    static func vibrate(style: VibrateStyle = .light) {
        switch style {        
        case .light:
            let generator = UIImpactFeedbackGenerator(style: .light)
            generator.impactOccurred()
        case .medium:
            let generator = UIImpactFeedbackGenerator(style: .medium)
            generator.impactOccurred()
        case .heavy:
            let generator = UIImpactFeedbackGenerator(style: .heavy)
            generator.impactOccurred()
        default:
            let generator = UISelectionFeedbackGenerator()
            generator.selectionChanged()
        }
        
    }
}
