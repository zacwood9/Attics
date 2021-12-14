//
//  RecordingViewController.swift
//  Attics
//
//  Created by Zachary Wood on 12/26/20.
//  Copyright © 2020 Zachary Wood. All rights reserved.
//

import UIKit
import SwiftUI
import Combine
import Network

enum ViewState {
    case loading
    case success([Song])
    case failure
}

class RecordingViewController : UIViewController, ObservableObject {
    let viewModel: RecordingViewModel
    private let musicPlayer: MusicPlayer

    private lazy var scrollView = UIScrollView()

    private lazy var hostingController = UIHostingController(
        rootView: RecordingView(viewModel: viewModel, musicPlayer: musicPlayer)
    )

    init(viewModel: RecordingViewModel, musicPlayer: MusicPlayer) {
        self.viewModel = viewModel
        self.musicPlayer = musicPlayer
        super.init(nibName: nil, bundle: nil)

        navigationItem.largeTitleDisplayMode = .never
        navigationItem.title = ""
    }

    override func viewDidLoad() {
        scrollView.translatesAutoresizingMaskIntoConstraints = false
        hostingController.view.translatesAutoresizingMaskIntoConstraints = false
        
//        view.addSubview(scrollView)
        scrollView.backgroundColor = .systemBackground
        view.backgroundColor = .systemBackground

        addChild(hostingController)
        view.addSubview(hostingController.view)
        hostingController.didMove(toParent: self)

        NSLayoutConstraint.activate([
//            scrollView.topAnchor.constraint(equalTo: view.topAnchor),
//            scrollView.bottomAnchor.constraint(equalTo: view.bottomAnchor),
//            scrollView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
//            scrollView.trailingAnchor.constraint(equalTo: view.trailingAnchor),

            hostingController.view.topAnchor.constraint(equalTo: view.topAnchor),
            hostingController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            hostingController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            hostingController.view.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])

        navigationItem.rightBarButtonItem = UIBarButtonItem(image: UIImage(systemName: "square.and.arrow.up"), style: .plain, target: self, action: #selector(share))

        viewModel.load()
    }

    required init?(coder: NSCoder) {
        fatalError()
    }

    @objc func share() {
        guard let stored = viewModel.storage.getStoredRecording(for: viewModel.recording) else { return }
        let item = ShareItem(stored)
        let ac = UIActivityViewController(activityItems: [item], applicationActivities: nil)
        if let popoverController = ac.popoverPresentationController {
            popoverController.sourceRect = CGRect(x: UIScreen.main.bounds.width / 2, y: UIScreen.main.bounds.height / 2, width: 0, height: 0)
            popoverController.sourceView = self.view
            popoverController.permittedArrowDirections = UIPopoverArrowDirection(rawValue: 0)
        }
        present(ac, animated: true)
    }
}

class ShareItem: NSObject, UIActivityItemSource {
    let stored: StoredRecording

    init(_ stored: StoredRecording) {
        self.stored = stored
    }

    var urlString: String {
        "https://attics.io/ShowRecording?identifier=\(stored.recording.identifier)"
    }

    func activityViewControllerPlaceholderItem(_ activityViewController: UIActivityViewController) -> Any {
        return URL(string: "https://attics.io/ShowRecording?identifier=\(stored.recording.identifier)")!
    }

    func activityViewController(_ activityViewController: UIActivityViewController, itemForActivityType activityType: UIActivity.ActivityType?) -> Any? {
        switch activityType {
        case .some(.postToTwitter):
            return "Check out this \(stored.band.name) show on @AtticsApp! \(urlString)"
        case .some(.message), .some(.mail), .some(.postToFacebook):
            return "Check out this \(stored.band.name) show on Attics! \(urlString)"
        default: return URL(string: urlString)!
        }
    }


}

enum WarningState {
    case notWarned
    case warning
    case warned
}

class RecordingViewModel: ObservableObject {
    let band: Band
    let performance: Show
    let recording: Source
    let storage: AppStorageManager
    let api: APIClient
    let monitor: NWPathMonitor
    private let _songTapped: (StoredRecording, Song) -> ()

    @Published var favorite: Bool
    @Published var state: ViewState = .loading
    @Published var downloadState: DownloadState
    var warningState: WarningState = .notWarned
    @Published var modalOpen = false
    var isWarningModal = false

    var songs: [Song] {
        switch state {
        case .success(let songs): return songs
        default: return []
        }
    }

    private var downloadStateCancellable: AnyCancellable?
    private var favoritesCancellable: AnyCancellable?
    private var networkCancellable: AnyCancellable?

    init(band: Band,
         performance: Show,
         recording: Source,
         api: APIClient,
         storage: AppStorageManager,
         songTapped: @escaping (StoredRecording, Song) -> ()
    ) {
        self.band = band
        self.performance = performance
        self.recording = recording
        self.api = api
        self.storage = storage
        self.favorite = storage.favorited.map(\.recording).contains(recording)
        self._songTapped = songTapped

        self.monitor = NWPathMonitor()
        self.monitor.start(queue: .main)

        let alreadyDownloaded = storage.downloaded.map(\.recording).contains(recording)
        if alreadyDownloaded {
            self.downloadState = .downloaded
        } else if let progress = storage.downloadProgresses.first(where: { $0.0.id == recording.id }) {
            self.downloadState = .downloading(progress.1)
        } else {
            self.downloadState = .notDownloaded
        }

        downloadStateCancellable = storage.recordingsPublisher
            .map { $0.first(where: { $0.id == recording.id }) }
            .compactMap({ $0 })
            .map { $0.downloadState }
            .receive(on: DispatchQueue.main)
            .sink(receiveValue: { downloadState in
                self.downloadState = downloadState
            })

        favoritesCancellable = storage.favoritedPublisher
            .map { $0.map(\.recording) }
            .map { $0.contains(recording) }
            .receive(on: DispatchQueue.main)
            .assign(to: \.favorite, on: self)
    }

    func load() {
        if let stored = storage.getStoredRecording(for: recording), !stored.songs.isEmpty {
            state = .success(stored.songs)
            print("LOG got songs from cache")
        } else {
            networkCancellable = api.getRecording(identifier: recording.identifier)
            .receive(on: DispatchQueue.main)
            .sink(receiveCompletion: { completion in
            }, receiveValue: { [weak self] response in
                self?.state = .success(response.songs)
                self?.cacheRecording(response.songs)
            })
        }
    }

    private func cacheRecording(_ songs: [Song]) {
        storage.addRecording(band: band, performance: performance, recording: recording, songs: songs)
    }

    func favoriteClick() {
        storage.addFavorite(band: band, performance: performance, recording: recording)
        UIDevice.vibrate()
    }

    func downloadClick() {
        switch warningState {
        case .notWarned:
            if monitor.currentPath.isExpensive {
                modalOpen = true
                isWarningModal = true
            } else {
                warningState = .warned
                downloadClick()
            }
        default:
            switch downloadState {
            case .downloading: cancelDownload()
            case .downloaded:
                modalOpen = true
                isWarningModal = false
            case .notDownloaded: startDownload()
            }
        }

    }

    private func cancelDownload() {
        storage.cancelDownload(for: recording)
        UIDevice.vibrate()
    }

    func removeDownload() {
        storage.removeDownload(for: recording)
        UIDevice.vibrate()
    }

    private func startDownload() {
        storage.startDownload(recording: recording)
        UIDevice.vibrate()
    }

    func songTapped(_ song: Song) {
        guard let stored = storage.getStoredRecording(for: recording) else { fatalError() }
        _songTapped(stored, song)
        UIDevice.vibrate(style: .select)
    }

    deinit {
        print("LOG RecordingViewModel.deinit")
    }
}

struct RecordingView : View {
    @ObservedObject var viewModel: RecordingViewModel
    @ObservedObject var musicPlayer: MusicPlayer

    var body: some View {
        ScrollView {
        VStack {
            PlayerHeader(viewModel: viewModel)
            Divider()
            switch viewModel.state {
            case .success:
                VStack {
                    SongList(viewModel: viewModel, musicPlayer: musicPlayer, songClick: viewModel.songTapped)

                    Button(action: {
                        UIApplication.shared.open(URL(string: "https://archive.org/details/\(viewModel.recording.identifier)")!)
                    }, label: {
                        Text("View on archive.org")
                            .padding()
                    }).padding([.leading, .trailing], 8)
                    .foregroundColor(.white)
                    .background(Color(#colorLiteral(red: 0.1986990605, green: 0.2647938419, blue: 0.5506226206, alpha: 1)))
                    .cornerRadius(8)
                    .padding(.top, 16)
                }
            default:
                Spacer()
                LoadingComponent(retry: nil)
                Spacer()
            }
        }
        }
        .actionSheet(isPresented: $viewModel.modalOpen) {
            if viewModel.isWarningModal {
                let nvm = ActionSheet.Button.cancel(Text("Never mind!")) {
                    viewModel.modalOpen = false
                }
                let remove = ActionSheet.Button.default(Text("Yes, download")) {
                    viewModel.warningState = .warned
                    viewModel.removeDownload()
                    viewModel.downloadClick()
                }
                return ActionSheet(title: Text("Downloading shows can use a lot of data. It is recommended you first connect to WiFi. Are you sure you would like to download over cellular?"), message: nil, buttons: [remove, nvm])
            } else {
                let nvm = ActionSheet.Button.cancel(Text("Never mind!")) { viewModel.modalOpen = false }
                let remove = ActionSheet.Button.destructive(Text("Yes, remove download")) { viewModel.removeDownload() }
                return ActionSheet(title: Text("Remove download?"), message: nil, buttons: [remove, nvm])
            }
        }
    }
}

struct PlayerHeader : View {
    @ObservedObject var viewModel: RecordingViewModel

    var body: some View {
        VStack {
            HStack {
                Spacer()
                VStack {
                    Text(viewModel.band.name)
                        .font(.title)
                        .bold()
                    if viewModel.performance.venue != "Unknown" {
                        Text("Live at \(viewModel.performance.venue)")
                            .font(.title2)
                    }

                    Text(viewModel.performance.date)
                        .font(.title3)
                }
                Spacer()
            }.padding(EdgeInsets(top: 12, leading: 0, bottom: 0, trailing: 0))

            HStack {
                FavoriteButton(viewModel: viewModel)
                DownloadButton(viewModel: viewModel)
            }
        }
    }
}

struct ShareButton: View {
    let onClick: () -> ()

    var body: some View {
        Button(action: onClick) {
            HStack {
                Image(systemName: "square.and.arrow.up")
                Text("Share")
            }.foregroundColor(Color.orange)
        }
        .padding()
        .background(Color.init(.systemBackground))
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(Color.orange, lineWidth: 1.5)
        )
        .padding()
    }
}

struct FavoriteButton: View {
    let onClick: () -> ()
    let isFavorite: Bool

    var body: some View {
        return Button(action: onClick) {
            HStack {
                Image(systemName: isFavorite ? "heart.fill" : "heart").foregroundColor(isFavorite ? Color.white : Color.red)
                Text(isFavorite ? "Unfavorite" : "Favorite")
            }.foregroundColor(isFavorite ? Color.white : Color.red)
        }
        .padding()
        .background(isFavorite ? Color.red : Color.init(.systemBackground))
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(Color.red, lineWidth: 1.5)
        )
        .padding()

    }

    init(onClick: @escaping () -> () = {}, isFavorite: Bool = false) {
        self.onClick = onClick
        self.isFavorite = isFavorite
    }

    init(viewModel: RecordingViewModel) {
        self.init(onClick: viewModel.favoriteClick, isFavorite: viewModel.favorite)
    }
}

struct DownloadButton: View {
    let onClick: () -> ()
    let downloadState: DownloadState

    var body: some View {
        switch downloadState {
        case .downloaded: HStack {
            Button(action: onClick) {
                Text("Downloaded")
            }.foregroundColor(.white)
            .padding()
            .background(Color.green)
            .cornerRadius(8)
            .padding()

        }
        case .downloading: HStack {
            Button(action: onClick) {
                HStack {
                    Text("Downloading...")
                }
            }.foregroundColor(.accentColor)
            .padding()
            .background(Color.init(.systemBackground))
            .cornerRadius(8)
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(Color.accentColor, lineWidth: 1.5)
            )
            .padding()
        }
        case .notDownloaded:
            Button(action: onClick) {
                HStack {
                    Image(systemName: "square.and.arrow.down")
                    Text("Download")
                }
            }.foregroundColor(.accentColor)
            .padding()
            .background(Color.init(.systemBackground))
            .cornerRadius(8)
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(Color.accentColor, lineWidth: 1.5)
            )
            .padding()
        }

    }

    init(onClick: @escaping () -> () = {}, downloadState: DownloadState = .notDownloaded) {
        self.onClick = onClick
        self.downloadState = downloadState
    }

    init(viewModel: RecordingViewModel) {
        self.init(onClick: viewModel.downloadClick, downloadState: viewModel.downloadState)
    }
}

struct SongList: View {
    @ObservedObject var viewModel: RecordingViewModel
    @ObservedObject var musicPlayer: MusicPlayer
    var songClick: (Song) -> ()

    var body: some View {
        switch viewModel.state {
        case .success(let songs): list(songs)
        default: LoadingComponent(retry: nil).padding([.top], 64)
        }

    }

    func list(_ songs: [Song]) -> some View {
        return ForEach(songs.sorted { $0.track < $1.track }) { song in
            let playing = musicPlayer.state != nil && musicPlayer.state!.song == song
            let leadingView = playing ? AnyView(Image(systemName: "music.note")) : AnyView(Text("\(song.track)"))

            let title = Text(song.title)
                .font(.body)
                .foregroundColor(Color.init(.label))

            HStack {
                leadingView
                    .font(.footnote)
                    .foregroundColor(Color.init(.tertiaryLabel))
                    .frame(minWidth: 14, alignment: .trailing)
                title.lineLimit(1)
                Spacer()
                trailingView(song)
            }
            .padding(EdgeInsets(top: 8, leading: 12, bottom: 8, trailing: 12))
            .contentShape(Rectangle())
            .onTapGesture(count: 1, perform: { songClick(song) })
            Divider()
        }
    }

    @ViewBuilder func trailingView(_ song: Song) -> some View {
        if case .downloading(let progresses) = viewModel.downloadState, let p = progresses[song] {
            ProgressCircle(percentageFinished: p)
                .frame(width: 22, height: 22, alignment: /*@START_MENU_TOKEN@*/.center/*@END_MENU_TOKEN@*/)
        } else {
            Text(song.length)
                .font(.callout)
                .foregroundColor(Color.init(.secondaryLabel))
        }
    }
}



struct Recording_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            FavoriteButton().previewLayout(PreviewLayout.sizeThatFits)
            FavoriteButton(onClick: {}, isFavorite: true).previewLayout(PreviewLayout.sizeThatFits)

            DownloadButton(onClick: {}, downloadState: .notDownloaded)
                .previewLayout(PreviewLayout.sizeThatFits)
            DownloadButton(onClick: {}, downloadState: .downloading([ : ]))
                .previewLayout(PreviewLayout.sizeThatFits)
            DownloadButton(onClick: {}, downloadState: .downloaded)
                .previewLayout(PreviewLayout.sizeThatFits)
        }
    }
}

struct SourceResponse: Codable {
    var band: Band
    var performance: Show
    var recording: Source
    var songs: [Song]
}
