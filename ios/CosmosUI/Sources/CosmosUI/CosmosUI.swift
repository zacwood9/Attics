import SwiftUI

public struct RatingView_: UIViewRepresentable {
    let rating: Double
    
    public func makeUIView(context: Context) -> CosmosUIView {
        let cosmosView = CosmosUIView()
        cosmosView.frame = CGRect(x: 0, y: 0, width: 23 * 5, height: 23)
        cosmosView.translatesAutoresizingMaskIntoConstraints = false
        cosmosView.settings.fillMode = .precise
        cosmosView.settings.starSize = 23
        cosmosView.settings.starMargin = 0
        cosmosView.rating = rating
        cosmosView.settings.updateOnTouch = false
        return cosmosView
    }
    
    public func updateUIView(_ uiView: CosmosUIView, context: Context) {
    }
}

public struct CosmosView : View {
    public var rating: Double
    
    public init(rating: Double) {
        self.rating = rating
    }
    
    public var body : some View {
        RatingView_(rating: rating)
            .frame(width: 23 * 5, height: 23)
    }
}

struct CosmosView_Previews: PreviewProvider {
    static var previews: some View {
        CosmosView(rating: 4)
    }
}
