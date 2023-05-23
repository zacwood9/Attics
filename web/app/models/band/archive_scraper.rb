# frozen_string_literal: true

require 'async'
require 'async/semaphore'

require 'rest-client'
require 'internet_archive'

class Band::ArchiveScraper
  attr_reader :band, :insert_queue, :update_queue

  def initialize(band)
    @band = band
    @insert_queue = []
    @update_queue = []
  end

  def scrape
    @dates_id_map = @band.performances.pluck(:id, :date).reduce({}) { |acc, (id, date)| acc[date.iso8601] = id; acc }
    @identifiers_id_map = @band.recordings.pluck(:id, :identifier).reduce({}) { |acc, (id, identifier)| acc[identifier] = id; acc }

    cursor = nil

    loop do
      url = scrape_url(@band.collection, cursor)
      Rails.logger.info("getting: #{url}")
      response = RestClient.get(url)
      result = JSON.parse(response.body)

      items = result["items"]
      items.each(&method(:process))

      cursor = result["cursor"]
      break if cursor.blank?
    end

    process_updates
    process_inserts
  end

  private

  def process(item)
    date = item["date"]&.split('T')&.first
    if date.blank?
      Rails.logger.info("ArchiveScraper: missing date, #{item.to_s}")
      return
    end

    begin
      date.to_date
    rescue Date::Error => _
      Rails.logger.warn("found invalid date for #{item["identifier"]}: #{date}}. skipping")
      return
    end

    city, state = item["coverage"]&.split(', ')
    venue = item["venue"]
    if @dates_id_map.key? date
      update_attrs = { city: city, state: state, venue: item["venue"].presence }.compact
      Performance.update(@dates_id_map[date], update_attrs) if update_attrs.present?
    else
      add_performance(date: date, venue: venue, city: city, state: state)
    end

    if @identifiers_id_map.key? item["identifier"]
      queue_for_update item
    else
      queue_for_insert item
    end
  end

  def add_performance(date:, venue:, city:, state:)
    performance = @band.performances.create!(date: date, venue: venue, city: city, state: state)
    @dates_id_map[date] = performance.id
  end

  def queue_for_update(item)
    @update_queue << item
  end

  def queue_for_insert(item)
    @insert_queue << item
  end

  def process_updates
    @update_queue.each_slice(1000, &method(:process_update_batch))
  end

  def process_update_batch(slice)
    attrs = slice.filter_map do |item|
      identifier = item["identifier"]

      recording_id = @identifiers_id_map[identifier]
      if recording_id.blank?
        Rails.logger.warn("updating #{identifier}, but couldn't find it's recording id? skipping...")
        next
      end

      date = item["date"]&.split('T')&.first
      performance_id = @dates_id_map[date]
      if performance_id.blank?
        Rails.logger.warn("updating #{identifier}, but couldn't find it's performance on #{date}? skipping...")
        next
      end

      {
        id: recording_id,
        performance_id: performance_id,
        lineage: item["lineage"],
        identifier: identifier,
        transferer: item["transferer"],
        source: item["source"],
        archive_downloads: item["downloads"] || 0,
        avg_rating: item["avg_rating"] || 0,
        num_reviews: item["num_reviews"] || 0,
      }
    end

    Recording.upsert_all attrs, returning: false
  end

  def process_inserts
    @insert_queue.each_slice(1000, &method(:process_insert_batch))
  end

  def process_insert_batch(slice)
    attrs = slice.map do |item|
      date = item["date"]&.split('T')&.first
      {
        performance_id: @dates_id_map[date],
        lineage: item["lineage"],
        identifier: item["identifier"],
        transferer: item["transferer"],
        source: item["source"],
        archive_downloads: item["downloads"] || 0,
        avg_rating: item["avg_rating"] || 0,
        num_reviews: item["num_reviews"] || 0,
      }
    end

    new_recordings = Recording.insert_all! attrs, returning: %w[id identifier]
    new_recordings.each do |recording|
      @identifiers_id_map[recording["identifier"]] = recording["id"]
    end

    Async do
      semaphore = Async::Semaphore.new(5)

      attrs.map do |item|
        semaphore.async do
          attempts ||= 1
          files = ::InternetArchive.files(item[:identifier])

          track_attributes = Track.attributes_from_files(@identifiers_id_map[item[:identifier]], files)

          if track_attributes.blank?
            Recording.destroy(@identifiers_id_map[item[:identifier]])
          else
            Track.insert_all!(track_attributes)
          end

        rescue Net::TimeoutError
          Rails.logger.warn("timeout, retrying once...")
          attempts += 1
          retry unless attempts > 2
        end
      end
    end.wait
  end

  def scrape_url(collection, cursor = nil)
    url = "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:#{collection}"
    url += "&cursor=#{cursor}" if cursor.present?
    url
  end
end
