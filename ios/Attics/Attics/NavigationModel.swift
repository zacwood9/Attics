//
//  Navigation.swift
//  Attics
//
//  Created by Zachary Wood on 12/21/23.
//

import Foundation
import AtticsCore
import SwiftUI
import Combine

struct BandDestination: Codable, Hashable, Equatable {
    let bandId: String
    let bandName: String
}

struct YearDestination: Codable, Hashable, Equatable {
    let bandId: String
    let year: String
}

struct PerformanceDestination: Codable, Hashable, Equatable {
    let performanceId: String
    let performanceDate: String
}

struct RecordingDestination: Codable, Hashable, Equatable {
    let recordingId: String
}

enum Navigation: Codable, Hashable, Equatable {
    case band(BandDestination)
    case year(YearDestination)
    case performance(PerformanceDestination)
    case recording(RecordingDestination)
    case storedRecording(RecordingDestination)
}

enum LibraryNavigation: Codable, Hashable {
    case recording(RecordingDestination)
    case history
}

enum Tab: Codable, Equatable, Hashable {
    case browse
    case library
    case settings
}

class NavigationModel: ObservableObject {    
    @Published private(set) var tab: Tab
    @Published var homeDestinations: [Navigation]
    @Published var libraryDestinations: [LibraryNavigation]
    
    private var tabCancellable: AnyCancellable?
    private var homeDestinationsCancellable: AnyCancellable?
    private var libraryDestinationsCancellable: AnyCancellable?
    
    var tabBinding: Binding<Tab> {
        Binding(get: { self.tab }, set: { self.setTab($0) })
    }
    
    init() {
        tab = (try? app.persistence.loadDecodable(at: .selectedTab)) ?? .browse
        homeDestinations = (try? app.persistence.loadDecodable(at: .navigation)) ?? []
        libraryDestinations = (try? app.persistence.loadDecodable(at: .libraryNavigation)) ?? []
        
        tabCancellable = $tab.sink { tab in
            Task.detached {
                do {
                    try app.persistence.persistEncodable(tab, to: .selectedTab)
                } catch {
                    logger.error("Failed to persist selected tab: \(error)")
                }
            }
        }
                
        homeDestinationsCancellable = $homeDestinations.sink { homeDestinations in
            Task.detached {
                do {
                    try app.persistence.persistEncodable(homeDestinations, to: .navigation)
                } catch {
                    logger.error("Failed to persist HomeTab navigation: \(error)")
                }
            }
        }
        
        libraryDestinationsCancellable = $libraryDestinations.sink { libraryDestinations in
            Task.detached {
                do {
                    try app.persistence.persistEncodable(libraryDestinations, to: .libraryNavigation)
                } catch {
                    logger.error("Failed to persist LibraryTab navigation: \(error)")
                }
            }
        }
    }
    
    func setTab(_ tab: Tab) {
        if self.tab == tab {
            switch tab {
            case .browse: homeDestinations = []
            case .library: libraryDestinations = []
            case .settings: break
            }
        } else {
            self.tab = tab
        }
    }

}
