require "rest-client"
require "internet_archive"

class InitialScrapeJob < ApplicationJob
  queue_as :default

  around_perform do |_job, block|
    ActiveRecord::Base.transaction do
      block.call
    end
  end

  def perform(band_id)
    band = Band.find(band_id)
    if band.performances.any?
      Rails.logger.info "Exiting Initial Scrape: #{band.name} already has data loaded"
      return
    end

    cursor = nil

    recordings = []
    loop do
      response = RestClient.get(scrape_url(band.collection, cursor))
      result = JSON.parse(response.body)

      items = result["items"]
      items.each { |item| recordings.push(build_attrs(band, item)) }

      cursor = result["cursor"]
      break if cursor.blank?
    end

    recordings.each do |recording|
      files = InternetArchive.files(recording.identifier)
      track_attributes = Track.attributes_from_files(recording.id, files)
      next if track_attributes.blank?

      Track.insert_all!(track_attributes)
    end
  end

  def build_attrs(band, item)
    date = item["date"]&.split('T')&.first
    if date.blank?
      Rails.logger.info("InitialScrape: missing date, #{item.to_s}")
      return
    end

    performance = Performance.find_or_create_by!(band: band, date: date)

    city, state = item["coverage"]&.split(', ')
    update_attrs = { city: city, state: state, venue: item["venue"].presence }.compact
    performance.update!(update_attrs) if update_attrs.present?

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

  def scrape_url(collection, cursor = nil)
    url = "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:#{collection}"
    url += "&cursor=#{cursor}" if cursor.present?
    url
  end
end
