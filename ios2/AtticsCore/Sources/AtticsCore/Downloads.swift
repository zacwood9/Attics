//
//  File.swift
//
//
//  Created by Zachary Wood on 12/25/23.
//

import Foundation
import SQLite

public class Downloads: NSObject, ObservableObject, DownloaderDelegate, URLSessionDownloadDelegate {
    let p: Persistence
    
    @Published public private(set) var fileDownloadProgresses = [String : DownloadItem]()
    @Published public private(set) var activeDownloadIdentifiers = Set<String>()
    @Published public private(set) var downloadedRecordingIds: Set<String>
    
    private var downloaders: Set<Downloader> = []
    
    private lazy var urlSession: URLSession = {
        let config = URLSessionConfiguration.background(withIdentifier: "me.zacwood.Attics.Downloads")
        config.isDiscretionary = false
        let session = URLSession(configuration: config, delegate: self, delegateQueue: .main)
        return session
    }()
    
    init(p: Persistence) throws {
        self.p = p
        downloadedRecordingIds = Set(try p.recordingRepository.getDownloads().map(\.id))
    }
    
    public func reloadDownloadedRecordings() {
        do {
            downloadedRecordingIds = Set(try p.recordingRepository.getDownloads().map(\.id))
        } catch {
            logger.error("Failed to reload recordings: \(error)")
        }
    }
    
    public func removeAllDownloads() throws {
        for subfolder in p.downloads.subfolders {
            try subfolder.delete()
        }
        
        try p.recordingRepository.removeAllDownloads()
        downloadedRecordingIds = []
    }
    
    public func addDownloader(recordingId: String, identifier: String) -> Downloader {
        let downloader = Downloader(p: p, recordingId: recordingId, identifier: identifier, urlSession: urlSession, delegate: self)
        downloaders.insert(downloader)
        activeDownloadIdentifiers.insert(identifier)
        return downloader
    }
    
    public func cancelDownloader(identifier: String) {
        guard let downloader = downloaders.first(where: { $0.identifier == identifier }) else { return }
        downloaders.remove(downloader)
        activeDownloadIdentifiers.remove(identifier)
        downloader.items.forEach { fileDownloadProgresses.removeValue(forKey: $0.fileName) }
        downloader.cancel()
        
        logger.info("Canceled download for Recording(identifier: \(identifier)).")
    }
    
    public func removeDownload(id: String) {
        downloadedRecordingIds.remove(id)
        do {
            if let recording = try p.recordingRepository.markUndownloaded(id: id) {
                try p.downloads.subfolder(named: recording.identifier).delete()
            }
        } catch {
            logger.error("Failed to remove download for Recording(id: \(id)): \(error)")
        }
        
        
        logger.info("Removed download for Recording(id: \(id)).")
    }
    
    func downloadItemFinished(downloader: Downloader, item: DownloadItem) {
        logger.info("Finished downloading file \(item.fileName).")
    }
    
    func downloadItemProgressed(downloader: Downloader, item: DownloadItem) {
        fileDownloadProgresses[item.fileName] = item
    }
    
    func downloaderFinished(downloader: Downloader) {
        downloaders.remove(downloader)
        activeDownloadIdentifiers.remove(downloader.identifier)
        downloader.items.forEach { fileDownloadProgresses.removeValue(forKey: $0.fileName) }
        
        if let recording = try? p.recordingRepository.markDownloaded(id: downloader.recordingId) {
            downloadedRecordingIds.insert(recording.id)
        }
        
        logger.info("Finished downloading Recording(identifier: \(downloader.identifier)).")
    }
    
    public func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didWriteData bytesWritten: Int64, totalBytesWritten: Int64, totalBytesExpectedToWrite: Int64) {
        
        downloaders.forEach { downloader in
            downloader.urlSession(session, downloadTask: downloadTask, didWriteData: bytesWritten, totalBytesWritten: totalBytesWritten, totalBytesExpectedToWrite: totalBytesExpectedToWrite)
        }
    }
    
    public func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        downloaders.forEach { downloader in
            downloader.urlSession(session, downloadTask: downloadTask, didFinishDownloadingTo: location)
        }
    }
}
