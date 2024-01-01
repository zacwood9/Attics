//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/24/23.
//

import Foundation
import Combine

public enum DownloadState {
    case waiting
    case downloading(Double)
    case finished
}

public struct BatchDownload {
    public let identifier: String
    public let fileNames: [String]
    
    public init(identifier: String, fileNames: [String]) {
        self.identifier = identifier
        self.fileNames = fileNames
    }
}

public class DownloadItem: Equatable {
    public static func == (lhs: DownloadItem, rhs: DownloadItem) -> Bool {
        return lhs.identifier == rhs.identifier && lhs.fileName == rhs.fileName
    }
    
    public let identifier: String
    public let fileName: String
    public let task: URLSessionDownloadTask
    public var bytesWritten: Int64 = 0
    public var bytesExpected: Int64? = nil
    public var estimatedTimeRemaining: TimeInterval? = nil
    public var finished = false
    
    init(identifier: String, fileName: String, task: URLSessionDownloadTask) {
        self.identifier = identifier
        self.fileName = fileName
        self.task = task
    }
}

protocol DownloaderDelegate {
    func downloadItemFinished(downloader: Downloader, item: DownloadItem)
    func downloadItemProgressed(downloader: Downloader, item: DownloadItem)
    func downloaderFinished(downloader: Downloader)
}

fileprivate var urlSessions = [String : URLSession]()

public class Downloader: NSObject {
    public let recordingId: String
    public let identifier: String
    
    private let p: Persistence
    private let urlSession: URLSession
    private let delegate: DownloaderDelegate
    
    var items: [DownloadItem] = []
    
    init(p: Persistence, recordingId: String, identifier: String, urlSession: URLSession, delegate: DownloaderDelegate) {
        self.p = p
        self.recordingId = recordingId
        self.identifier = identifier
        self.urlSession = urlSession
        self.delegate = delegate
    }
    
    public func download(batchDownload: BatchDownload) {
        let identifier = batchDownload.identifier
 
        items = batchDownload.fileNames.map { fileName in
            let url = URL(string: "https://archive.org/download/\(identifier)/\(fileName)".addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!)!
            let request = URLRequest(url: url)
            let task = urlSession.downloadTask(with: request)
            task.resume()
            
            logger.info("Started download for Track(fileName: \(fileName)).")
            return DownloadItem(identifier: identifier, fileName: fileName, task: task)
        }
    }
    
    public func cancel() {
        for item in items {
            item.task.cancel()
        }
        items = []        
    }
    
    public func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didWriteData bytesWritten: Int64, totalBytesWritten: Int64, totalBytesExpectedToWrite: Int64) {
                
        if let item = items.first(where: { $0.task == downloadTask }) {
            item.bytesWritten = totalBytesWritten
            item.bytesExpected = totalBytesExpectedToWrite
            item.estimatedTimeRemaining = downloadTask.progress.estimatedTimeRemaining
            
            delegate.downloadItemProgressed(downloader: self, item: item)
        }
    }
    
    public func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        if let item = items.first(where: { $0.task == downloadTask }) {
            do {
                try self.p.registerDownload(sourceUrl: location, identifier: item.identifier, fileName: item.fileName)
                delegate.downloadItemFinished(downloader: self, item: item)
                item.finished = true
                            
                if items.allSatisfy(\.finished) {
                    delegate.downloaderFinished(downloader: self)
                }
            } catch {
                logger.error("Failed to move download for Track(fileName: \(item.fileName)) to storage: \(error).")
            }
        }
    }
}
