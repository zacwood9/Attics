//
//  UIFont+Extensions.swift
//  Attics
//
//  Created by Zachary Wood on 6/28/18.
//  Copyright © 2018 Zachary Wood. All rights reserved.
//

import UIKit
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
//        layer.shadowColor = UIColor.lightGray.cgColor
//        layer.shadowOffset = CGSize(width: 0, height: 0)
//        layer.shadowRadius = 2.0
//        layer.shadowOpacity = 1.0
//        layer.masksToBounds = false;
//        layer.shadowPath = UIBezierPath(roundedRect: bounds, cornerRadius: layer.cornerRadius).cgPath        
    }
    
    func roundCorners() {
        layer.cornerRadius = 8.0
        layer.borderWidth = 1.0
        layer.borderColor = UIColor.clear.cgColor
        layer.masksToBounds = true;
    }
}


extension UIViewController {
    func openSafari(to url: URL) {
        UIApplication.shared.open(url)
    }
    
    func presentAlert(with message: String) {
        let alertVC = UIAlertController(title: "An error has occured.", message: message + "\n Please try again later.", preferredStyle: .alert)
        alertVC.addAction(UIAlertAction(title: "Ok", style: .cancel, handler: nil))
        
        DispatchQueue.main.async { [weak self] in
            if let tabBarVC = self?.tabBarController {
                tabBarVC.present(alertVC, animated: true, completion: nil)
            } else {
                self?.present(alertVC, animated: true, completion: nil)
            }
        }
    }
    
    func presentAlert(with actions: [UIAlertAction]) {
        let alertVC = UIAlertController(title: "Sort By", message: nil, preferredStyle: .actionSheet)
        actions.forEach { alertVC.addAction($0) }
        alertVC.addAction(UIAlertAction(title: "Cancel", style: .cancel, handler: nil))
        
        if let popoverController = alertVC.popoverPresentationController { // handle iPads
            popoverController.sourceView = view
        }
        
        self.tabBarController?.present(alertVC, animated: true, completion: nil)
    }
}

extension UIViewController {
    func add(_ child: UIViewController) {
        addChild(child)
        view.addSubview(child.view)
        child.didMove(toParent: self)
    }
    
    func add(_ child: UIViewController, to subview: UIView) {
        addChild(child)
        subview.addSubview(child.view)
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

enum SFSymbol: String {
    case heartFill = "heart.fill"
    case heart = "heart"
    case downloaded = "checkmark.circle.fill"
    case download = "square.and.arrow.down"
    case wifiSlash = "wifi.slash"
    case play = "play.fill"
    case pause = "pause.fill"
    case forward = "forward.fill"
    case backward = "backward.fill"
    case close = "xmark.circle.fill"
    case musicNoteList = "music.note.list"
}

extension UIImage {
    convenience init(icon: SFSymbol) {
        self.init(systemName: icon.rawValue)!
    }
    
    convenience init(icon: SFSymbol, size: CGFloat = 32) {
        self.init(systemName: icon.rawValue, withConfiguration: UIImage.SymbolConfiguration(pointSize: size, weight: .bold))!
    }
}


extension UIColor {
    static let atticsBlue = #colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)
}

extension Folder {
    static var applicationSupport: Folder {
        let path = "Library/Application Support"
        guard Folder.home.containsSubfolder(at: path) else {
            return try! Folder.home.createSubfolder(at: path)
        }
        return try! Folder.home.subfolder(at: path)
    }
}

extension File {
    func excludeFromBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        try! urlCopy.setResourceValues(vals)
    }
}

extension Folder {
    func excludeForBackup() {
        var vals = URLResourceValues()
        vals.isExcludedFromBackup = true
        var urlCopy = url
        try! urlCopy.setResourceValues(vals)
    }
}
