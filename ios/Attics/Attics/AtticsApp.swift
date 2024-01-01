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

@main
struct AtticsApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self)
    var appDelegate
    
    @StateObject var navigationModel = NavigationModel()
    @StateObject var networkMonitor = NetworkMonitor()
    
    var body: some Scene {
        WindowGroup {
            TabView(selection: navigationModel.tabBinding) {
                HomeTab(path: $navigationModel.homeDestinations)
                    .tabItem { Label("Home", systemImage: "house.fill") }
                    .tag(Tab.browse)
                
                LibraryTab(path: $navigationModel.libraryDestinations)
                    .tabItem { Label("My Library", systemImage: "heart.fill")}
                    .tag(Tab.library)
                
                SettingsTab()
                    .tabItem { Label("More", systemImage: "ellipsis") }
                    .tag(Tab.settings)
            }
            .atticsPopup(navigationModel)
            .atticsPersistTrackPlays()
            .atticsCancelDownloadsOnLossOfNetwork()
            .atticsLegacyImport()
            .atticsUpdatePopover()
            .atticsDeepLink(navigationModel)
            .environmentObject(networkMonitor)
        }
    }
}

extension View {
    func atticsPopup(_ navigationModel: NavigationModel) -> some View {
        self.modifier(AtticsPopupBar(navigationModel: navigationModel))
    }
}

struct AtticsPopupBar: ViewModifier {
    @ObservedObject var navigationModel: NavigationModel
    @StateObject var audioSystem = app.audioSystem
    
    @State private var isPopupOpen = false
    
    var popupBinding: Binding<Bool> {
        Binding(get: { audioSystem.playlist != nil }, set: { _ in })
    }
    
    func body(content: Content) -> some View {
        content
            .popup(isBarPresented: popupBinding, isPopupOpen: $isPopupOpen) {
                if let playlist = audioSystem.playlist {
                    PopupView(playlist: playlist, openRecording: openRecording)
                } else {
                    EmptyView()
                }
            }
            .popupBarStyle(.floating)
    }
    
    private func openRecording(id: String) {
        isPopupOpen = false
        
        let destination: Navigation
        if let _ = try? app.library.loadItem(recordingId: id) {
            destination = .storedRecording(RecordingDestination(recordingId: id))
        } else {
            destination = .recording(RecordingDestination(recordingId: id))
        }
        
        if navigationModel.tab != .browse {
            navigationModel.setTab(.browse)
        }
        
        if navigationModel.homeDestinations.last != destination {
            navigationModel.homeDestinations.append(destination)
        }
    }
}

extension View {
    func atticsPersistTrackPlays() -> some View {
        self.modifier(AtticsPersistTrackPlays())
    }
}

struct AtticsPersistTrackPlays: ViewModifier {
    @StateObject var audioSystem = app.audioSystem
    
    func body(content: Content) -> some View {
        content
            .onReceive(audioSystem.trackIdPlayed) { trackId in
                Task.detached(priority: .utility) {
                    logger.info("Played \(trackId)")
                    app.history.recordEntry(for: trackId)
                }
            }
    }
}

extension View {
    func atticsNavigationBar(_ title: String? = nil) -> some View {
        self.modifier(AtticsNavigationBar(title: title))
    }
}

struct AtticsNavigationBar: ViewModifier {
    let title: String?
    
    func body(content: Content) -> some View {
        content
            .toolbarBackground(Color.atticsBlue, for: .navigationBar)
            .toolbarBackground(.visible, for: .navigationBar)
            .toolbarColorScheme(.dark, for: .navigationBar)
            .navigationTitle(title ?? "")
    }
}

extension View {
    func atticsCancelDownloadsOnLossOfNetwork() -> some View {
        self.modifier(AtticsCancelDownloads())
    }
}

struct AtticsCancelDownloads: ViewModifier {
    @StateObject var downloads = app.downloads
    @EnvironmentObject var networkMonitor: NetworkMonitor
    
    @State var showAlert = false
    
    func body(content: Content) -> some View {
        content
            .onReceive(networkMonitor.$status, perform: { status in
                if status == .disconnected && !downloads.activeDownloadIdentifiers.isEmpty {
                    downloads.activeDownloadIdentifiers.forEach { downloads.cancelDownloader(identifier: $0) }
                    showAlert = true
                }
            })
            .alert(isPresented: $showAlert) {
                Alert(
                    title: Text("Lost connection while downloading"),
                    message: Text("Active downloads have been canceled.")
                )
            }
    }
}

extension View {
    func atticsDeepLink(_ navigationModel: NavigationModel) -> some View {
        self.modifier(AtticsDeepLink(navigationModel: navigationModel))
    }
}

struct AtticsDeepLink: ViewModifier {
    @ObservedObject var navigationModel: NavigationModel
    
    func body(content: Content) -> some View {
        content
            .onOpenURL { url in
                if url.pathComponents.starts(with: ["/", "api", "legacy", "recordings"]) && url.pathComponents.count > 4 {
                    let identifier = url.pathComponents[4]
                    navigationModel.setTab(.browse)
                    navigationModel.homeDestinations.append(
                        .recording(RecordingDestination(recordingId: identifier))
                    )
                }
                
                if url.pathComponents.starts(with: ["/", "recordings"]) && url.pathComponents.count > 2 {
                    let recordingId = url.pathComponents[2]
                    navigationModel.setTab(.browse)
                    navigationModel.homeDestinations.append(
                        .recording(RecordingDestination(recordingId: recordingId))
                    )
                }
            }
    }
}

extension View {
    func atticsUpdatePopover() -> some View {
        self.modifier(AtticsUpdatePopover())
    }
}

struct AtticsUpdatePopover: ViewModifier {
    @AppStorage("v2-update") var showUpdateModal = true
    @State var showingUpdateModal = false
    
    func body(content: Content) -> some View {
        content
            .popover(isPresented: $showingUpdateModal) { UpdateView(dismiss: { showingUpdateModal = false }) }
            .onAppear {
                if showUpdateModal {
                    showingUpdateModal = true
                    showUpdateModal = false
                }
            }
    }
}

extension View {
    func atticsLegacyImport() -> some View {
        self.modifier(AtticsLegacyImport())
    }
}

struct AtticsLegacyImport: ViewModifier {
    @StateObject var legacyImport = app.legacyImport
    
    @State var inProgress = false
    @State var failureError: Error?
    
    var failureBinding: Binding<Bool> {
        Binding(get: { failureError != nil }, set: { show in if !show { failureError = nil } })
    }
    
    func body(content: Content) -> some View {
        content
            .onAppear {
                Task.detached {
                    let importNeeded = await legacyImport.importNeeded()
                    guard importNeeded else { return }
                    
                    logger.info("Legacy import needed. Running in background...")
                    await MainActor.run { inProgress = true }
                    
                    do {
                        try await app.legacyImport.run()
                        await MainActor.run {
                            inProgress = false
                            app.downloads.reloadDownloadedRecordings()
                            app.favorites.reloadFavoritedRecordingIds()
                        }
                    } catch {
                        await MainActor.run {
                            logger.error("Failed to run legacy import: \(error)")
                            inProgress = false
                            failureError = error
                        }
                    }
                }
            }
            .popover(isPresented: failureBinding) {
                VStack {
                    VStack {
                        Image(systemName: "wifi.exclamationmark").font(.system(size: 48))
                        Text("An error occured while migrating your data.").bold()
                        if let failureError {
                            Text(failureError.localizedDescription).font(.footnote)
                        }
                        Divider()
                            .padding(.vertical)
                        Text("Don't worry, your saved shows are still there. Attics v2.0 requires an internet connection on first launch to migrate your library to a new internal format before you'll see them. Please force quit the app, make sure your internet is working, and try again.").multilineTextAlignment(.leading)
                        Divider()
                            .padding(.vertical)
                        Text("It's also possible Attics is down right now, or there was another error. If your internet is working and you're still seeing this error, please wait and try again, or send me an email at zac.wood@hey.com.").multilineTextAlignment(.leading)
                    }.padding([.horizontal], 24)
                }
            }
    }
}

extension UIDevice {
    enum VibrateStyle {
        case light, medium, heavy, select
    }
    static func vibrate(style: VibrateStyle = .light) {
        switch style {
        case .light:
            let generator = UIImpactFeedbackGenerator(style: .light)
            generator.impactOccurred()
        case .medium:
            let generator = UIImpactFeedbackGenerator(style: .medium)
            generator.impactOccurred()
        case .heavy:
            let generator = UIImpactFeedbackGenerator(style: .heavy)
            generator.impactOccurred()
        default:
            let generator = UISelectionFeedbackGenerator()
            generator.selectionChanged()
        }        
    }
}

extension Array where Element : Equatable {
    func filter(matchesAny: [(Element) -> Bool]) -> [Element] {
        return filter {
            for f in matchesAny {
                if f($0) {
                    return true
                }
            }
            return false
        }
    }
}

import Network

enum NetworkStatus {
    case disconnected
    case cellular
    case wifi
}

class NetworkMonitor: ObservableObject {
    private let networkMonitor = NWPathMonitor()
    private let workerQueue = DispatchQueue(label: "Monitor")
    @Published var status = NetworkStatus.disconnected

    init() {
        networkMonitor.pathUpdateHandler = { [weak self] path in
            guard let self else { return }
            
            Task { @MainActor in
                if path.status == .satisfied {
                    self.status = path.isExpensive ? .cellular : .wifi
                } else {
                    self.status = .disconnected
                }
            }
        }
        networkMonitor.start(queue: workerQueue)
    }
}
