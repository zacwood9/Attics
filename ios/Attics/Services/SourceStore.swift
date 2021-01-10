//
//  FavoritesStore.swift
//  Attics
//
//  Created by Zachary Wood on 7/10/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import UIKit
import Combine

protocol SourceStore {
    func save(source: LegacySource)
    func remove(source: LegacySource)
    func getAll() -> [LegacySource]
    func contains(source: LegacySource) -> Bool
}

struct FileSystemSourceStore: SourceStore {
    let file: File
    
    var favs: Set<LegacySource> {
        return try! Set(JSONDecoder().decode([LegacySource].self, from: file.read()))
    }
    
    init(named name: String, backup: Bool = false) {
        print(Folder.home.path)
        let cacheDir = Folder.applicationSupport
        file = try! cacheDir.createFileIfNeeded(at: name)
        
        if !backup {
            file.excludeFromBackup()
        }
        
        let favsStr = try! file.readAsString()
        if favsStr == "" {
            try! file.write("[]")
        }
    }
    
    func save(source: LegacySource) {
        let newFavs = favs.union([source])
        do {
            try file.write(JSONEncoder().encode(Array(newFavs)))
            NotificationCenter.default.post(name: .FavoritesChanged, object: source.identifier)
        } catch {
            print(error)
        }
    }
    
    func remove(source: LegacySource) {
        let newFavs = favs.subtracting([source])
        do {
            try file.write(JSONEncoder().encode(Array(newFavs)))
            NotificationCenter.default.post(name: .FavoritesChanged, object: source.identifier)
        } catch {
            print(error)
        }
    }
    
    func getAll() -> [LegacySource] {
        if let favs = try? JSONDecoder().decode([LegacySource].self, from: file.read()) {
            return favs
        } else {
            print("failed to load favorites")
            return []
        }
    }
    
    func contains(source: LegacySource) -> Bool {
        return favs.contains(source)
    }
    
}

struct DownloadMetadata {
    var manager: DownloadManager_
    var stored: StoredRecording
}

class AppStorageManager {
    /// All recordings persisted on system.
    private lazy var _recordings: CurrentValueSubject<[StoredRecording], Never> = {
        let folder = Folder.applicationSupport
        if let file = try? folder.file(named: "storage.json") {
            let data = try! file.read()
            let value = try! JSONDecoder().decode([StoredRecording].self, from: data)
            var newValues = [StoredRecording]()
            for stored in value {
                var copy = stored
                switch copy.downloadState {
                case .downloading(_): copy.downloadState = .notDownloaded
                default: break
                }
                newValues.append(copy)
            }
            return CurrentValueSubject(newValues)
        }
        
        return CurrentValueSubject([])
    }()
    
    private lazy var _band: CurrentValueSubject<Band, Never> = {
        let folder = Folder.applicationSupport
        if let file = try? folder.file(named: "band.json"),
           let data = try? file.read(),
           let value = try? JSONDecoder().decode(Band.self, from: data) {
            return CurrentValueSubject(value)
        }
        return CurrentValueSubject(Band(collection: "GratefulDead", name: "Grateful Dead", logoUrl: ""))
    }()
    
    var downloadManagers = [String : (DownloadManager_, AnyCancellable)]()
    
    init() {
        let f = Folder.applicationSupport
        try! f.createSubfolderIfNeeded(withName: "Downloads")
    }
    
    var band: Band {
        get { _band.value }
        set { _band.value = newValue }
    }
    
    var bandPublisher: AnyPublisher<Band, Never> {
        _band.eraseToAnyPublisher()
    }
    
    var recordings: [StoredRecording] {
        _recordings.value
    }
    
    var recordingsPublisher: AnyPublisher<[StoredRecording], Never> {
        _recordings.eraseToAnyPublisher()
    }
    
    var downloadedPublisher: AnyPublisher<[StoredRecording], Never> {
        _recordings.map { values in
            return values.filter { $0.downloadState == .downloaded || self.downloadManagers[$0.id] != nil }
        }.eraseToAnyPublisher()
    }

    var downloaded: [StoredRecording] {
        recordings.filter { $0.downloadState == .downloaded }
    }
    
    var favoritedPublisher: AnyPublisher<[StoredRecording], Never>  {
        _recordings.map { values in
            return values.filter { $0.favorite }
        }.eraseToAnyPublisher()
    }
    
    var favorited: [StoredRecording] {
        recordings.filter { $0.favorite }
    }
    
    var downloadProgresses: [(StoredRecording, [Song:Double])] {
        recordings
            .map { storedRecording in
                switch storedRecording.downloadState {
                case .downloading(let state):
                    return (storedRecording, state)
                default:
                    return nil
                }
            }.compactMap { $0 }
    }
    
    var downloadProgressesPublisher: AnyPublisher<[(StoredRecording, [Song:Double])], Never> {
        _recordings
            .map { values in
                return values.map {
                    switch $0.downloadState {
                    case .downloading(let state): return ($0, state)
                    default: return nil
                    }
                }.compactMap { $0 }
            }
            .eraseToAnyPublisher()
    }
    
    func startDownload(recording: Source) {
        guard var stored = getStoredRecording(for: recording) else { fatalError() }
        let manager = DownloadManager_(storage: self, recording: recording)
        stored.downloadState = .downloading([:])
        updateStoredRecording(stored)
        
        let cancellable = manager.publisher
            .receive(on: DispatchQueue.main)
            .sink { completion in
                print("received completion")
                var s = self.getStoredRecording(for: recording)!
                s.downloadState = .downloaded
                self.updateStoredRecording(s)
            } receiveValue: { [weak self] in
                self?.updateProgresses(recording, $0)
            }
        manager.start()
        downloadManagers[recording.id] = (manager, cancellable)
    }
    
    private func updateProgresses(_ recording: Source, _ newProgress: (Song, Double)) {
        let (song, percentageFinished) = newProgress
        guard var stored = getStoredRecording(for: recording) else { return }
        
        switch stored.downloadState {
        case .notDownloaded:
            stored.downloadState = .downloading([song:percentageFinished])
            self.updateStoredRecording(stored)
        case .downloading(let progresses):
            let a = progresses.merging([song : percentageFinished], uniquingKeysWith: { $1 })
            stored.downloadState = .downloading(a)
            self.updateStoredRecording(stored)
        default:
            break
        }
    }
    
    func cancelDownload(for recording: Source) {
        guard var stored = getStoredRecording(for: recording) else { return }
        guard let (manager, cancellable) = downloadManagers[stored.id] else { return }
        manager.cancel()
        cancellable.cancel()
        downloadManagers[stored.id] = nil
        stored.downloadState = .notDownloaded
        updateStoredRecording(stored)
    }
    
    func removeDownload(for recording: Source) {
        guard var stored = getStoredRecording(for: recording) else { return }
        stored.downloadState = .notDownloaded
        updateStoredRecording(stored)
        
        if let base = try? Folder.applicationSupport.subfolder(named: "Downloads"),
           let recordingFolder = try? base.createSubfolderIfNeeded(withName: stored.recording.identifier) {
            recordingFolder.files.forEach { try! $0.delete() }
        }
    }
    
    func removeAllDownloads() throws {
        let base = try Folder.applicationSupport.subfolder(named: "Downloads")
        
        for stored in downloaded {
            let folder = try base.subfolder(named: stored.recording.identifier)
            try folder.delete()
        }
        
        _recordings.value = recordings.map { stored in
            var copy = stored
            switch stored.downloadState {
            case .downloaded, .downloading:
                copy.downloadState = .notDownloaded
            default:
                break
            }
            return copy
        }
    }
    
    @discardableResult
    func addRecording(band: Band, performance: Show, recording: Source, songs: [Song]) -> StoredRecording {
        if let stored = getStoredRecording(for: recording) { return stored }
        let newEntry = StoredRecording(
            band: band,
            performance: performance,
            recording: recording,
            songs: songs,
            favorite: false,
            downloadState: .notDownloaded
        )
        _recordings.value.append(newEntry)
        return newEntry
    }
    
    func addFavorite(band: Band, performance: Show, recording: Source) {
        guard var stored = getStoredRecording(for: recording) else { fatalError() }
        stored.favorite.toggle()
        updateStoredRecording(stored)
    }
    
    func getStoredRecording(for recording: Source) -> StoredRecording? {
        return _recordings.value.first(where: { $0.id == recording.id })
    }
    
    func updateStoredRecording(_ recording: StoredRecording) {
        guard let index = _recordings.value.firstIndex(where: { $0.id == recording.id }) else { fatalError("updating record not found" )}
        // updating the index directly doesn't always trigger a change, so copy and reassign
        // to ensure Combine sends the new value to the publishers
        var copy = _recordings.value
        copy[index] = recording
        _recordings.value = copy
    }
    
    func saveToDisk() {
        let folder = Folder.applicationSupport
        let recordingsToSave = _recordings.value.filter { $0.favorite || $0.downloadState == .downloaded }
        if let data = try? JSONEncoder().encode(recordingsToSave) {
            let file = try! folder.createFileIfNeeded(withName: "storage.json")
            try! file.write(data)
        } else {
            print("unable to save recordings to disk")
        }
        
        if let data = try? JSONEncoder().encode(_band.value) {
            let file = try! folder.createFileIfNeeded(withName: "band.json")
            try! file.write(data)
        } else {
            print("unable to save band to disk")
        }
        
        print("saved to disk")
    }
    
    func getLocalSongUrl(recording: Source, song: Song) -> URL? {
        if let base = try? Folder.applicationSupport.subfolder(named: "Downloads"),
           let recordingFolder = try? base.subfolder(named: recording.identifier),
           let songFile = try? recordingFolder.file(named: song.fileName) {
            return songFile.url
        }
        return nil
    }
    
    func moveDownloadToStorage(_ url: URL, _ stored: StoredRecording, _ song: Song) {
        if let base = try? Folder.applicationSupport.subfolder(named: "Downloads"),
           let recordingFolder = try? base.createSubfolderIfNeeded(withName: stored.recording.identifier),
           let tempFile = try? File(path: url.path) {
            try! tempFile.move(to: recordingFolder)
            try? tempFile.rename(to: song.fileName, keepExtension: false)
        }
    }
    
}

enum DownloadState : Codable, Equatable {
    case notDownloaded
    case downloaded
    case downloading([Song : Double])
    
    init(from decoder: Decoder) throws {
        let v = try decoder.singleValueContainer()
        if let d = try? v.decode(String.self) {
            switch d {
            case "downloaded": self = .downloaded
            case "not_downloaded": self = .notDownloaded
            default: throw DecodingError.dataCorruptedError(in: v, debugDescription: "unable to decode download state")
            }
        } else if let d = try? v.decode([Song : Double].self) {
            self = .downloading(d)
        } else {
            throw DecodingError.dataCorruptedError(in: v, debugDescription: "unable to decode download state")
        }
    }
    
    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch self {
        case .downloaded: try container.encode("downloaded")
        case .notDownloaded: try container.encode("not_downloaded")
        case .downloading(let d): try container.encode(d)
        }
    }
}

/// Manages downloading a recording
class DownloadManager_ {
    let storage: AppStorageManager
    let storedRecording: StoredRecording
    
    var songQueue: [Song] = []
    
    private var currentTasks: [DownloadTask_] = []
    private var currentCancellables: [AnyCancellable] = []
    private let _publisher = PassthroughSubject<(Song, Double), Error>()
    
    init(storage: AppStorageManager, recording: Source) {
        self.storage = storage
        self.storedRecording = storage.recordings.first(where: { $0.id == recording.id })! // TODO: force unwrap
    }
    
    var publisher: AnyPublisher<(Song, Double), Error> {
        _publisher.eraseToAnyPublisher()
    }
    
    func start() {
        songQueue = storedRecording.songs
        downloadFromQueue()
    }
    
    func cancel() {
        currentTasks.forEach { $0.cancel() }
        currentCancellables.forEach { $0.cancel() }
    }
    
    private func downloadFromQueue() {
        if let song = songQueue.first {
            songQueue.removeFirst()
            
            let url = "https://archive.org/download/\(storedRecording.recording.identifier)/\(song.fileName)"
            let currentTask = DownloadTask_(
                url: URL(string: url.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!)!,
                onComplete: { self.storage.moveDownloadToStorage($0, self.storedRecording, song) }
            )
            let currentCancellable = currentTask.publisher_
                .receive(on: DispatchQueue.main)
                .sink { completion in
                switch completion {
                case .finished:
                    let _ = self.currentTasks.popLast()
                    if self.currentTasks.isEmpty {
                        self._publisher.send(completion: .finished)
                    }
                case .failure(let error): print("ERROR: \(error)")
                }
            } receiveValue: { percentageFinished in
                self._publisher.send((song, percentageFinished))
            }
            
            currentTask.resume()
            currentTasks.append(currentTask)
            currentCancellables.append(currentCancellable)
            
            downloadFromQueue()
        }
    }
}

struct StoredRecording : Codable, Equatable, Hashable {
    static func == (lhs: StoredRecording, rhs: StoredRecording) -> Bool {
        lhs.id == rhs.id
    }
    
    var id: String {
        recording.id
    }
    
    var band: Band
    var performance: Show
    var recording: Source
    var songs: [Song]
    var favorite: Bool
    var downloadState: DownloadState
    
    var hashValue: Int { id.hashValue }
    func hash(into hasher: inout Hasher) {
        id.hash(into: &hasher)
    }
}

class DownloadTask_: NSObject, URLSessionDownloadDelegate {
    private var downloadTask: URLSessionDownloadTask!
    private let _publisher = PassthroughSubject<Double, Error>()
    private let onComplete: (URL) -> ()
    
    var publisher_: AnyPublisher<Double, Error> {
        _publisher.eraseToAnyPublisher()
    }
    
    init(url: URL, onComplete: @escaping (URL) -> ()) {
        self.onComplete = onComplete
        super.init()
        self.downloadTask = NetworkManager.shared.registerTask(self, url: url)
    }
    
    func resume() {
        self.downloadTask.resume()
    }
    
    func cancel() {
        downloadTask.cancel()
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        _publisher.send(completion: .finished)
        onComplete(location)
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didWriteData bytesWritten: Int64, totalBytesWritten: Int64, totalBytesExpectedToWrite: Int64) {
        print(Double(totalBytesWritten) / Double(totalBytesExpectedToWrite))
        _publisher.send(Double(totalBytesWritten) / Double(totalBytesExpectedToWrite))
    }
    
    func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
        if let error = error {
            _publisher.send(completion: .failure(error))
        } else if let error = task.error {
            _publisher.send(completion: .failure(error))
        } else {
            _publisher.send(completion: .failure(URLError(.badServerResponse)))
        }
    }
}

class NetworkManager: NSObject, URLSessionDownloadDelegate {
    static var shared = NetworkManager()
    
    lazy var urlSession: URLSession = {
        let config = URLSessionConfiguration.background(withIdentifier: "me.zacwood.Attics.1")
        config.isDiscretionary = false
        return URLSession(configuration: config, delegate: self, delegateQueue: .main)
    }()
    
    private var downloadTasks = [Int : DownloadTask_]()
    private override init() { }
    
    func registerTask(_ task: DownloadTask_, url: URL) -> URLSessionDownloadTask {
        let t = urlSession.downloadTask(with: url)
        downloadTasks[t.taskIdentifier] = task
        return t
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        guard let registeredTask = downloadTasks[downloadTask.taskIdentifier] else {
            print("unregistered task finished")
            return
        }
        registeredTask.urlSession(session, downloadTask: downloadTask, didFinishDownloadingTo: location)
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didWriteData bytesWritten: Int64, totalBytesWritten: Int64, totalBytesExpectedToWrite: Int64) {
        guard let registeredTask = downloadTasks[downloadTask.taskIdentifier] else {
            print("wrote data for unregistered task: cancelling")
            downloadTask.cancel()
            return
        }
        registeredTask.urlSession(session, downloadTask: downloadTask, didWriteData: bytesWritten, totalBytesWritten: totalBytesWritten, totalBytesExpectedToWrite: totalBytesExpectedToWrite)
    }
    
    func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
        guard let registeredTask = downloadTasks[task.taskIdentifier] else {
            return
        }
        registeredTask.urlSession(session, task: task, didCompleteWithError: error)
        downloadTasks[task.taskIdentifier] = nil
    }
}

extension Notification.Name {
    public static let FavoriteWasAdded = Notification.Name("FavoriteWasAdded")
    public static let FavoriteWasRemoved = Notification.Name("FavoriteWasRemoved")
    public static let FavoritesChanged = Notification.Name("FavoriteWasRemoved")
}
