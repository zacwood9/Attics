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
    let app: AtticsCore
    
    @Published var destinations: [Navigation]
    var destinationsBinding: Binding<[Navigation]> {
        Binding() {
            self.destinations
        } set: {
            self.destinations = $0
            do {
                try self.app.persistence.persistEncodable($0, to: .navigation)
            } catch {
                print(error)
            }
            
        }
    }
    
    @Published var libraryDestinations: [LibraryNavigation]
    var libraryDestinationsBinding: Binding<[LibraryNavigation]> {
        Binding() {
            self.libraryDestinations
        } set: {
            self.libraryDestinations = $0
            do {
                try self.app.persistence.persistEncodable($0, to: .libraryNavigation)
            } catch {
                print(error)
            }
        }
    }
    
    init(app: AtticsCore) {
        self.app = app
        destinations = (try? app.persistence.loadDecodable(at: .navigation)) ?? []
        libraryDestinations = (try? app.persistence.loadDecodable(at: .libraryNavigation)) ?? []
    }

}
