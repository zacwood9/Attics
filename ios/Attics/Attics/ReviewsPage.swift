//
//  ReviewsPage.swift
//  Attics
//
//  Created by Zachary Wood on 1/6/24.
//

import SwiftUI
import AtticsCore
import CosmosUI

struct ReviewsPage: View {
    enum SortKeys: String, CaseIterable, Hashable, Equatable {
        case dateAsc = "Date (asc)"
        case dateDesc = "Date (desc)"
    }
    
    static let sortFuncs: [SortKeys : (APIReview, APIReview) -> Bool] = [
        .dateAsc: { $0.reviewdate < $1.reviewdate },
        .dateDesc: { $0.reviewdate > $1.reviewdate }
    ]
    
    let archiveIdentifier: String
    @State private var result: APIResult<APIReviewsPage> = .loading
    @State private var sortBy = SortKeys.dateDesc
    
    var body: some View {
        ResultView(result) { reviews in
            ReviewsView(reviews: sortedReviews(reviews.result))
        }
        .atticsNavigationBar("Reviews")
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            Menu("Sort by: \(sortBy.rawValue)") {
                ForEach(SortKeys.allCases, id: \.self) { key in
                    Button(key.rawValue) { sortBy = key }
                }
            }
        }
        .task {
            do {
                let reviews = try await app.apiClient.getReviews(archiveIdentifier: archiveIdentifier)
                self.result = .success(reviews)
            } catch {
                print(error)
                self.result = .error(error)
            }
        }
    }
    
    func sortedReviews(_ reviews: [APIReview]) -> [APIReview] {
        reviews.sorted(by: Self.sortFuncs[sortBy]!)
    }
}

struct ReviewsView: View {
    let reviews: [APIReview]
    
    var body: some View {
        List {
            Section("\(reviews.count) reviews") {
                ForEach(reviews) { review in
                    VStack(alignment: .leading, spacing: 12) {
                        HStack {
                            VStack(alignment: .leading) {
                                Text(review.reviewtitle ?? "Review").font(.headline)
                                Text(review.reviewdate).font(.footnote)
                                Text(review.reviewer).font(.footnote)
                            }
                            Spacer()
                            CosmosView(rating: Double(review.stars) ?? 0)
                        }
                        
                        Text(review.reviewbody).font(.body)
                    }
                }
            }
        }
    }
}

#Preview {
    NavigationStack {
        ReviewsPage(archiveIdentifier: "gd77-05-08.sbd.hicks.4982.sbeok.shnf")
            .atticsNavigationBar("Reviews")
    }
}

#Preview {
    NavigationStack {
        ReviewsView(reviews: previewReviews)
            .atticsNavigationBar("Reviews")
        
    }
}

fileprivate let previewReviews = [
    APIReview(reviewId: "asdf", reviewbody: "This is an awesome review!", reviewtitle: "Wow!", reviewer: "Zac Wood", reviewdate: "2022-01-01", stars: "4.5"),
    APIReview(reviewId: "fdsa", reviewbody: "This show was terrible.\nI'd never listen to this again. This is a really long sentence to show the behavior of wrapping text.", reviewtitle: "Nope...", reviewer: "Zac Wood", reviewdate: "2022-01-02", stars: "1")
]
