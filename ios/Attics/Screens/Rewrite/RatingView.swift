//
//  RatingView.swift
//  Attics
//
//  Created by Zachary Wood on 12/16/23.
//  Copyright © 2023 Zachary Wood. All rights reserved.
//

import SwiftUI
import Cosmos

struct RatingView_: UIViewRepresentable {
    let rating: Double
    
    func makeUIView(context: Context) -> CosmosView {
        let cosmosView = CosmosView()
        cosmosView.frame = CGRect(x: 0, y: 0, width: 23 * 5, height: 23)
        cosmosView.translatesAutoresizingMaskIntoConstraints = false
        cosmosView.settings.fillMode = .precise
        cosmosView.settings.starSize = 23
        cosmosView.settings.starMargin = 0
        cosmosView.rating = rating
        cosmosView.settings.updateOnTouch = false
        return cosmosView
    }
    
    func updateUIView(_ uiView: CosmosView, context: Context) {
    }
}

struct RatingView : View {
    var rating: Double
    
    var body : some View {
        RatingView_(rating: rating)
            .frame(width: 23 * 5, height: 23)
    }
}

struct RatingView_Previews: PreviewProvider {
    static var previews: some View {
        RatingView(rating: 4)
    }
}
