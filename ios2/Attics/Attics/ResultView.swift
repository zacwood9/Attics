//
//  ResultView.swift
//  Attics
//
//  Created by Zachary Wood on 12/29/23.
//

import SwiftUI
import AtticsCore

struct ResultView<T: Decodable, Content: View>: View {
    let result: APIResult<T>
    @ViewBuilder let content: (_ data: T) -> Content
    
    init(_ result: APIResult<T>, content: @escaping (_: T) -> Content) {
        self.result = result
        self.content = content
    }
    
    var body: some View {
        switch result {
        case .loading:
            List {}.overlay {
                ProgressView()
            }
        case .success(let t):
            content(t)
        case .error(let error):
            List {}.overlay {
                VStack {
                    Text("Failed to load page")
                        .font(.headline)
                    Text(error.localizedDescription)
                        .font(.footnote)
                }
            }
        }
    }
}
