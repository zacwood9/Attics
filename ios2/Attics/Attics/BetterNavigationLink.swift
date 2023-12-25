import SwiftUI

struct BetterNavigationLink<Label: View,  P : Decodable & Encodable & Hashable>: View {
    let value: P?
    let label: () -> Label

    public init(value: P?, @ViewBuilder label: @escaping () -> Label) {
        self.value = value
        self.label = label
    }

    var body: some View {
        // HACK: ZStack with zero opacity + EmptyView
        // Hides default chevron accessory view for NavigationLink
        ZStack {
            NavigationLink(value: value) {
                EmptyView()
            }
            .opacity(0)

            self.label()
        }
    }
}
