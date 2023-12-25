//
//  File.swift
//  
//
//  Created by Zachary Wood on 12/16/23.
//

import Foundation

public struct BandWithMetadata: Decodable {
    public let id: String
    public let collection: String
    public let name: String
    public let logoUrl: String
    public let numPerformances: Int
    public let numRecordings: Int
}

public struct YearWithTopPerformances: Decodable {
    public let year: String
    public let topPerformances: [PerformanceWithMetadata]
}

public struct PerformanceWithMetadata: Decodable {
    public let id: String
    public let date: String
    public let city: String
    public let state: String
    public let venue: String
    public let numReviews: Int
    public let numRecordings: Int
    public let avgRating: Double
    public let bandId: String
    
    public var cityState: String {
        "\(city), \(state)"
    }
}

public struct APIBand: Decodable {
    public let id: String
    public let name: String
    public let collection: String
}

public struct APIPerformance: Decodable {
    public let id: String
    public let date: String
    public let city: String
    public let state: String
    public let venue: String
    public let bandId: String
}

public struct APIRecording: Decodable {
    public let id: String
    public let identifier: String
    public let transferer: String
    public let source: String
    public let avgRating: Double
    public let archiveDownloads: Int
    public let numReviews: Int
    public let lineage: String
    public let performanceId: String
}

public struct APIRecordingPage: Decodable {
    public let band: APIBand
    public let performance: APIPerformance
    public let recording: APIRecording
    public let tracks: [APITrack]
}

public struct APITrack: Decodable {
    public let id: String
    public let title: String
    public let fileName: String
    public let album: String
    public let track: Int
    public let length: String
    public let recordingId: String
    public let performanceId: String
    public let bandId: String
    public let downloadUrl: String
}

public struct APIReview: Decodable {
    public let reviewId: String
    public let reviewbody: String
    public let reviewtitle: String
    public let reviewer: String
    public let reviewdate: String
    public let stars: String
}
