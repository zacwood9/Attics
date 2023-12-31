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
}

enum LibraryNavigation: Codable, Hashable {
    case recording(RecordingDestination)
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
                
        homeDestinationsCancellable = $homeDestinations.sink { homeDestinations in
            do {
                try app.persistence.persistEncodable(homeDestinations, to: .navigation)
            } catch {
                logger.error("Failed to persist HomeTab navigation: \(error)")
            }
        }
        
        libraryDestinationsCancellable = $libraryDestinations.sink { libraryDestinations in
            do {
                try app.persistence.persistEncodable(libraryDestinations, to: .libraryNavigation)
            } catch {
                logger.error("Failed to persist LibraryTab navigation: \(error)")
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
