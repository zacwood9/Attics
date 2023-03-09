require "rest-client"
require "internet_archive"

class NightlyScrapeJob < ApplicationJob
  queue_as :default

  def perform(band_id)
    band = Band.find(band_id)
    Rails.logger.info("running nightly_scrape for #{band.name}...")

    results = advanced_search(band.collection)
    results.each do |item|
      recording = process_item(band, item)
      if recording.present?
        files = InternetArchive.files(recording.identifier)
        attributes = Track.attributes_from_files(recording.id, files)
        next if attributes.blank?

        Track.insert_all!(attributes)
      end
    end
  end

  def process_item(band, item)
    date = item["date"]&.split('T')&.first
    if date.blank?
      Rails.logger.info("InitialScrape: missing date, #{item.to_s}")
      return
    end

    performance = Performance.find_or_create_by!(band: band, date: date)

    city, state = item["coverage"]&.split(', ')
    update_attrs = { city: city, state: state, venue: item["venue"].presence }.compact
    performance.update!(update_attrs) if update_attrs.present?

    return if Recording.exists?(identifier: item["identifier"])

    Rails.logger.info("creating new recording: #{item["identifier"]}")
    Recording.create!(
      {
        performance_id: performance.id,
        lineage: item["lineage"],
        identifier: item["identifier"],
        transferer: item["transferer"],
        source: item["source"],
        archive_downloads: item["downloads"],
        avg_rating: item["avg_rating"],
        num_reviews: item["num_reviews"]
      }.compact
    )
  end
end

def advanced_search(collection, sort_by = "publicdate")
  base = "https://archive.org/advancedsearch.php"
  q = "q=collection:%22#{collection}%22"
  fields = "fl%5B%5D=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier,#{sort_by}"
  sort = "sort%5B%5D=#{sort_by}%20desc"
  output = "output=json"

  params = [q, fields, sort, output]
  url = base + "?" + params.join("&")

  JSON.parse(RestClient.get(url).body).dig("response", "docs") || []
end
