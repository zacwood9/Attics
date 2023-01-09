require "rest-client"

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

    loop do
      response = RestClient.get(scrape_url(band.collection, cursor))
      result = JSON.parse(response.body)

      items = result["items"]
      items.each { |item| build_attrs(band, item) }

      cursor = result["cursor"]
      break if cursor.blank?
    end

    Recording.find_each { |recording| create_songs(recording) }
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

  def create_songs(recording)
    response = RestClient.get("https://archive.org/metadata/#{recording.identifier}/files")
    result = JSON.parse(response.body)["result"] || []

    result
      .select { |file| file["name"]&.end_with?(".mp3") }
      .map { |file|
        # Sometimes derivative files are missing data that is present
        # on the file's source, so fetch both.
        if file["original"].present?
          [file, result.find { |other| other["name"] == file["original"] } || {}]
        else
          [file, {}]
        end
      }
      .each.with_index { |pair, i|
        file, original = pair
        Song.create!(
          file_name: file["name"],
          title: file["title"] || original["title"],
          track: i + 1,
          length: file["length"] || original["length"],
          creator: file["creator"] || original["creator"],
          album: file["album"] || original["album"],
          recording_id: recording.id,
        )
      }
  end

  def scrape_url(collection, cursor = nil)
    url = "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:#{collection}"
    url += "&cursor=#{cursor}" if cursor.present?
    url
  end
end