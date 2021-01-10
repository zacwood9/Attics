//
//  DownloadManager.swift
//  Attics
//
//  Created by Zachary Wood on 12/11/19.
//  Copyright © 2019 Zachary Wood. All rights reserved.
//

import Foundation
import Network

protocol DownloadManagerDelegate: class {
    func downloadProgress(_ manager: DownloadManager, for song: Song, with progress: Double)
    func downloadFinished(_ manager: DownloadManager, for song: Song)
    func downloadFailed(_ manager: DownloadManager, for song: Song)
}

fileprivate protocol DownloadDelegate: class {
    func downloadProgress(for song: Song, percentageFinished: Double)
    func downloadFinished(for song: Song)
    func downloadFailed(for song: Song)
}

class DownloadManager: DownloadDelegate {
    weak var delegate: DownloadManagerDelegate?
    
    let source: Source
    var progresses: [Song : Double] = [:]
    var downloaded: Bool {
        // metadata.json is created after all songs finish downloading,
        // so if it exists the source has been downloaded
        folder.containsFile(named: "metadata.json")
    }
    
    private let folder: Folder
    private var songs: [Song] = []
    private var tasks: [Song:DownloadTask] = [:]
    
    var isOnline: () -> Bool = { true }
    
    var downloadedSongs: Set<Song> = []
    
    var songMap: [String : Song] {
        songs.reduce(into: [String:Song]()) { result, song in
            result[song.fileName] = song
        }
    }
    
    init(source: Source) {
        self.source = source
        
        let cacheDir = Folder.applicationSupport
        let downloadsDir = try! cacheDir.createSubfolderIfNeeded(at: "Downloads")
        downloadsDir.excludeForBackup()
        folder = try! downloadsDir.createSubfolderIfNeeded(at: source.identifier)
    }
    
    func downloadAll(songs: [Song]) throws {
        self.songs = songs
        for song in songs {
            try download(song)
        }
    }
    
    func download(_ song: Song) throws {
        if isSongDownloaded(song) {
            print("Skipping: \(song.fileName)")
            downloadedSongs.insert(song)
            downloadFinished(for: song)
            return
        }
        
        print("Downloading \(song.title)")
        if let task = tasks[song] {
            _ = task.attemptResume()
        } else {
            let task = DownloadTask(in: folder, for: song, delegate: self)
            task.start()
            tasks[song] = task
            progresses[song] = 0
        }
    }
    
    func remove() {
        folder.files.forEach { try! $0.delete() }
        tasks.values.forEach { $0.cancel() }
        progresses = [:]
    }
    
    func downloadProgress(for song: Song, percentageFinished: Double) {
        DispatchQueue.main.async {
            self.progresses[song] = percentageFinished
            self.delegate?.downloadProgress(self, for: song, with: percentageFinished)
        }
    }
    
    func downloadFinished(for song: Song) {
        DispatchQueue.main.async {
            self.progresses.removeValue(forKey: song)
            self.tasks.removeValue(forKey: song)
            self.downloadedSongs.insert(song)
            if self.progresses.isEmpty {
                let metadata = try! self.folder.createFileIfNeeded(withName: "metadata.json")
                try! metadata.write(JSONEncoder().encode(self.songs))
            }
            self.delegate?.downloadFinished(self, for: song)
        }      
    }
    
    func downloadFailed(for song: Song) {
        guard !(tasks[song]?.attemptResume() ?? false) else { return }
        DispatchQueue.main.async {
            self.delegate?.downloadFailed(self, for: song)
        }
    }
    
    func isSongDownloaded(_ song: Song) -> Bool {
        return folder.containsFile(named: song.fileName)
    }
    
    func path(for song: Song) -> URL? {
        if isSongDownloaded(song) {
            return folder.url.appendingPathComponent(song.fileName)
        }
        return nil
    }
}

extension Source {
    var downloadFolder: Folder {
        let cacheDir = Folder.applicationSupport
        let downloadsDir = try! cacheDir.createSubfolderIfNeeded(at: "Downloads")
        return try! downloadsDir.createSubfolderIfNeeded(at: identifier)
    }
    
    var isDownloaded: Bool {
        downloadFolder.containsFile(named: "metadata.json")
    }
    
    var downloadedSongs: [Song] {
        (try? JSONDecoder().decode([Song].self, from: downloadFolder.file(named: "metadata.json").read())) ?? []
    }
}

extension Song {
    var fileURL: URL? {
//        if source.downloadFolder.containsFile(named: fileName) {
//            return source.downloadFolder.url.appendingPathComponent(fileName)
//        }
        return nil
    }
}

fileprivate class DownloadTask: NSObject, URLSessionDownloadDelegate {
    let folder: Folder
    let song: Song
    var urlSession: URLSession! = nil
    var delegate: DownloadDelegate?
    var resumeData: Data?
    var task: URLSessionDownloadTask?
    
    init(in folder: Folder, for song: Song, delegate: DownloadDelegate? = nil) {
        self.folder = folder
        self.song = song
        self.delegate = delegate
        super.init()
        
        let config = URLSessionConfiguration.background(withIdentifier: "me.zacwood.Attics.\(song.fileName)")
        config.isDiscretionary = false
        self.urlSession = URLSession(configuration: config, delegate: self, delegateQueue: nil)
    }
    
    func start() {
//        task = urlSession.downloadTask(with: song.downloadURL)
//        task?.resume()
    }
    
    func attemptResume() -> Bool {
        if let resumeData = resumeData {
            print("resuming \(song.title)")
            task = urlSession.downloadTask(withResumeData: resumeData)
            task?.resume()
            return true
        }
        return false
    }
    
    func cancel() {
        task?.cancel()
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didWriteData bytesWritten: Int64, totalBytesWritten: Int64, totalBytesExpectedToWrite: Int64) {
        delegate?.downloadProgress(for: song, percentageFinished: Double(totalBytesWritten)/Double(totalBytesExpectedToWrite))
    }
    
    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        delegate?.downloadFinished(for: song)
        do {
            try File(path: location.path).move(to: folder)
            try folder.file(named: location.lastPathComponent).rename(to: song.fileName, keepExtension: false)
        } catch {
            print("FAILED: \(error)")
        }
    }
    
    func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
        guard let error = error else { return }
        
        print("download failed for \(song.title)")
        if let error = error as? URLError {
            self.resumeData = error.downloadTaskResumeData
        }
        
        delegate?.downloadFailed(for: song)
    }
}


