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

public struct TopPerformancesPage: Decodable {
    public let onThisDay: [PerformanceWithMetadata]
    public let years: [YearWithTopPerformances]
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

public struct APIReviewsPage: Decodable {
    public let result: [APIReview]
}

public struct APIReview: Decodable, Identifiable {
    public var id: String {
        reviewId
    }
    
    public let reviewId: String
    public let reviewbody: String
    public let reviewtitle: String?
    public let reviewer: String
    public let reviewdate: String
    public let stars: String
    
    public init(reviewId: String?, reviewbody: String, reviewtitle: String?, reviewer: String, reviewdate: String, stars: String) {
        self.reviewId = reviewId ?? UUID().uuidString
        self.reviewbody = reviewbody
        self.reviewtitle = reviewtitle
        self.reviewer = reviewer
        self.reviewdate = reviewdate
        self.stars = stars
    }
    
    enum CodingKeys: CodingKey {
        case reviewId
        case reviewbody
        case reviewtitle
        case reviewer
        case reviewdate
        case stars
    }
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        self.reviewId = try container.decodeIfPresent(String.self, forKey: .reviewId) ?? UUID().uuidString
        self.reviewbody = try container.decode(String.self, forKey: .reviewbody)
        self.reviewtitle = try container.decodeIfPresent(String.self, forKey: .reviewtitle)
        self.reviewer = try container.decode(String.self, forKey: .reviewer)
        self.reviewdate = try container.decode(String.self, forKey: .reviewdate)
        self.stars = try container.decode(String.self, forKey: .stars)
    }
}

public struct APIMetadataPage: Decodable {
    public let result: APIMetadata
}

public struct APIMetadata: Decodable {
    public let identifier: String
    public let uploader: String?
    public let addeddate: String?
    public let date: String
    public let title: String?
    public let description: String?
    public let subject: String?
    public let venue: String?
    public let coverage: String?
    public let source: String?
    public let taper: String?
    public let transferer: String?
    public let notes: String?
    
    public init(identifier: String, uploader: String?, addeddate: String?, date: String, title: String?, description: String?, subject: String?, venue: String?, coverage: String?, source: String?, taper: String?, transferer: String?, notes: String?) {
        self.identifier = identifier
        self.uploader = uploader
        self.addeddate = addeddate
        self.date = date
        self.title = title
        self.description = description
        self.subject = subject
        self.venue = venue
        self.coverage = coverage
        self.source = source
        self.taper = taper
        self.transferer = transferer
        self.notes = notes
    }
    
    enum CodingKeys: CodingKey {
        case identifier
        case uploader
        case addeddate
        case date
        case title
        case description
        case subject
        case venue
        case coverage
        case source
        case taper
        case transferer
        case notes
    }
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        self.identifier = try container.decode(String.self, forKey: .identifier)
        self.uploader = try container.decodeIfPresent(String.self, forKey: .uploader)
        self.addeddate = try container.decodeIfPresent(String.self, forKey: .addeddate)
        self.date = try container.decode(String.self, forKey: .date)
        self.title = try container.decodeIfPresent(String.self, forKey: .title)
        self.description = try container.decodeIfPresent(String.self, forKey: .description)
        
        var subject = try? container.decodeIfPresent(String.self, forKey: .subject)
        if subject == nil, let subjects = try? container.decodeIfPresent([String].self, forKey: .subject) {
            subject = subjects.joined(separator: ", ")
        }
        self.subject = subject
        
        self.venue = try container.decodeIfPresent(String.self, forKey: .venue)
        self.coverage = try container.decodeIfPresent(String.self, forKey: .coverage)
        self.source = try container.decodeIfPresent(String.self, forKey: .source)
        self.taper = try container.decodeIfPresent(String.self, forKey: .taper)
        self.transferer = try container.decodeIfPresent(String.self, forKey: .transferer)
        self.notes = try container.decodeIfPresent(String.self, forKey: .notes)
    }
}
