//
//  ATSProgressCircle.swift
//  Attics
//
//  Created by Zachary Wood on 1/5/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI

class ATSProgressCircle: UIView {
    var trackLayer = CAShapeLayer()
    
    var percentageComplete: Double = 0.0 {
        didSet {
            setNeedsLayout()
        }
    }
    
    init(color: UIColor) {
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        trackLayer.strokeColor = color.cgColor
        configurePath()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }
    
    override func layoutSubviews() {
        configurePath()
    }

    private func configurePath() {
        let end = CGFloat(percentageComplete*(2 * .pi) - (0.5 * .pi))
        let circlePath = UIBezierPath(
            arcCenter: CGPoint(x: frame.size.width/2, y: frame.size.height/2),
            radius: min(frame.size.width, frame.size.height)/2,
            startAngle: CGFloat(-0.5 * .pi),
            endAngle: end,
            clockwise: true
        )
        trackLayer.path = circlePath.cgPath
        trackLayer.fillColor = UIColor.clear.cgColor
        trackLayer.lineWidth = 5.0
        trackLayer.strokeEnd = 1.0
        layer.addSublayer(trackLayer)
    }
}

struct ProgressCircle : UIViewRepresentable {
    var percentageFinished: Double
    
    func makeUIView(context: Context) -> ATSProgressCircle {
        let a = ATSProgressCircle(color: .systemOrange)
        a.percentageComplete = percentageFinished
        return a
    }
    
    func updateUIView(_ uiView: ATSProgressCircle, context: Context) {
        uiView.percentageComplete = percentageFinished
    }
}
