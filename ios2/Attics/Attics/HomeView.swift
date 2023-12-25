//
//  HomeView.swift
//  Attics
//
//  Created by Zachary Wood on 12/17/23.
//

import SwiftUI

struct HomeView: View {
    @StateObject var bandsViewModel = {
        let vm = BandsViewModel(app: app)
        return vm
    }()
    
    var body: some View {
        BandsView(viewModel: bandsViewModel)
            .navigationTitle("Attics")
            .task {
                await bandsViewModel.load()
            }
    }
}

#Preview {
    NavigationStack {
        HomeView()
    }
}
