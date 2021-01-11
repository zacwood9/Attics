//
//  DownloadManager.swift
//  Attics
//
//  Created by Zachary Wood on 12/11/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import Foundation
import Combine
import Network

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
