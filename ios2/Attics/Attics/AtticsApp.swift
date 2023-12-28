//
//  AtticsApp.swift
//  Attics
//
//  Created by Zachary Wood on 12/16/23.
//

import SwiftUI
import LNPopupUI
import AtticsCore
import Combine

let app = AtticsCore()

enum Tab: Equatable, Hashable {
    case browse
    case library
    case settings
}

@main
struct AtticsApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self)
    var appDelegate
    
    @StateObject var audioSystem = app.audioSystem
    @StateObject var favorites = app.favorites
    @StateObject var downloads = app.downloads
    @StateObject var navigationModel = NavigationModel(app: app)
    
    @State var selectedTab: Tab = .browse
    var selectionBinding: Binding<Tab> { Binding(
        get: {
            self.selectedTab
        },
        set: { newValue in
            if self.selectedTab == .browse && newValue == .browse {
                navigationModel.destinationsBinding.wrappedValue = []
            }
            
            if self.selectedTab == .library && newValue == .library {
                navigationModel.libraryDestinationsBinding.wrappedValue = []
            }
            
            self.selectedTab = newValue
        }
    )}
    
    var popupBinding: Binding<Bool> {
        Binding {
            audioSystem.playlist != nil
        } set: { _ in
            
        }
        
    }
    
    @State var isPopupOpen = false
    
    var body: some Scene {
        WindowGroup {
            TabView(selection: selectionBinding) {
                NavigationStack(path: navigationModel.destinationsBinding) {
                    HomeView()
                        .navigationDestination(for: Navigation.self) { destination in
                            switch destination {
                            case .band(let bandDestination):
                                BrowseView(bandId: bandDestination.bandId, bandName: bandDestination.bandName)
                            case .year(let yearDestination):
                                YearView(bandId: yearDestination.bandId, year: yearDestination.year)
                            case .performance(let destination):
                                PerformanceView(performanceId: destination.performanceId, performanceDate: destination.performanceDate)
                            case .recording(let destination):
                                RecordingView(recordingId: destination.recordingId)
                            }
                        }
                        .toolbarBackground(Color.atticsBlue, for: .navigationBar)
                        .toolbarBackground(.visible, for: .navigationBar)
                        .toolbarColorScheme(.dark, for: .navigationBar)
                }.tabItem {
                    Label("Home", systemImage: "house.fill")
                }.tag(Tab.browse)
                
                NavigationStack(path: navigationModel.libraryDestinationsBinding) {
                    LibraryView()
                        .toolbarBackground(Color.atticsBlue, for: .navigationBar)
                        .toolbarBackground(.visible, for: .navigationBar)
                        .toolbarColorScheme(.dark, for: .navigationBar)
                        .navigationDestination(for: LibraryNavigation.self) { destination in
                            switch destination {
                            case .recording(let destination):
                                LibraryItemView(recordingId: destination.recordingId)
                            }
                        }
                }
                .tabItem {
                    Label("My Library", systemImage: "heart.fill")
                }.tag(Tab.library)
                
                Text("WIP")
                    .tabItem {
                        Label("More", systemImage: "ellipsis")
                    }.tag(Tab.settings)
            }
            .popup(isBarPresented: popupBinding, isPopupOpen: $isPopupOpen) {
                if let playlist = audioSystem.playlist {
                    PopupView(playlist: playlist, openRecording: openRecording)
                } else {
                    EmptyView()
                }
            }
            .environmentObject(audioSystem)
            .environmentObject(favorites)
            .environmentObject(downloads)
        }
    }
    
    func openRecording(id: String) {
        isPopupOpen = false
        let destination: Navigation = .recording(RecordingDestination(recordingId: id))
        selectedTab = .browse
        if navigationModel.destinations.last != destination {
            navigationModel.destinations.append(destination)
        }
    }
}

