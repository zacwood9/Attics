//
//  SettingsTab.swift
//  Attics
//
//  Created by Zachary Wood on 12/29/23.
//

import StoreKit
import SwiftUI
import AtticsCore

struct SettingsTab: View {
    @Environment(\.openURL) var openURL
    @Environment(\.requestReview) var requestReview
    
    @State var showDeleteConfirmation = false
    @State var showClearConfirmation = false
    @State var showUpdatePopover = false
    
    var body: some View {
        SettingsView(
            openSafari: openSafari,
            openReview: openReview,
            removeAllDownloads: { showDeleteConfirmation = true },
            clearHistory: { showClearConfirmation = true },
            openUpdate: { showUpdatePopover = true }
        )
        .alert("Are you should you would like to delete all downloads?", isPresented: $showDeleteConfirmation) {
            Button("Cancel", role: .cancel) { }
            Button("Delete all downloads", role: .destructive) { removeAllDownloads() }
        }
        .alert("Are you should you would like to clear your listening history?", isPresented: $showClearConfirmation) {
            Button("Cancel", role: .cancel) { }
            Button("Clear listening history", role: .destructive) { clearHistory() }
        }
        .popover(isPresented: $showUpdatePopover) { UpdateView(dismiss: { showUpdatePopover = false }) }
    }
    
    private func openSafari(url: String) {
        guard let url = URL(string: url) else { return }
        openURL.callAsFunction(url)        
    }
    
    private func openReview() {
        Task {
            await requestReview.callAsFunction()
        }
    }
    
    private func removeAllDownloads() {
        do {
            try app.downloads.removeAllDownloads()
        } catch {
            logger.error("Failed to remove downloads: \(error)")
        }
    }
    
    private func clearHistory() {
        do {
            try app.history.clear()
        } catch {
            logger.error("Failed to clear listening history: \(error)")
        }
    }
}
