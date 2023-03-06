require 'pg'

bad_performance_id = '7ba03a47-6613-417b-bd65-3baf126212ab'
bad_recording_id = 'd1976b29-2c2b-434c-842b-6a1b1412432d'

ActiveRecord::Base.transaction do
  PG.connect(Rails.application.credentials[:legacy_database_url]) do |conn|
    unless Band.any?
      bands = conn.exec("select id,collection,name,logo_url from bands")
      attrs = bands.map do |band|
        {
          id: band["id"],
          collection: band["collection"],
          name: band["name"],
          logo_url: band["logo_url"]
        }
      end
      Band.insert_all!(attrs)
    end

    unless Performance.any?
      performances = conn.exec("select * from performances where id <> '#{bad_performance_id}'")
      attrs = performances.map do |performance|
        {
          id: performance["id"],
          date: performance["date"],
          venue: performance["venue"],
          city: performance["city"],
          state: performance["state"],
          band_id: performance["band_id"],
        }
      end
      Performance.insert_all!(attrs)
    end

    unless Recording.any?
      recordings = conn.exec("select * from recordings where performance_id <> '#{bad_performance_id}'")
      recordings.each_slice(1000).with_index do |items, i|
        attrs = items.map do |item|
          {
            id: item["id"],
            performance_id: item["performance_id"],
            lineage: item["lineage"],
            identifier: item["identifier"],
            transferer: item["transferer"],
            source: item["source"],
            archive_downloads: item["archive_downloads"],
            avg_rating: item["avg_rating"],
            num_reviews: item["num_reviews"]
          }
        end
        Recording.insert_all!(attrs.compact)
        pp "batch #{i} done"
      end
    end

    unless Track.any?
      tracks = conn.exec("select * from songs where recording_id <> '#{bad_recording_id}'")
      tracks.each_slice(1000).with_index do |items, i|
        attrs = items.map do |item|
          next if item['file_name'].blank?

          {
            id: item["id"],
            file_name: item["file_name"],
            title: item["title"],
            track: item["track"],
            length: item["length"],
            creator: item["creator"],
            album: item["album"],
            recording_id: item["recording_id"]
          }
        end
        Track.insert_all!(attrs.compact)
        pp "batch #{i} done"
      end
    end
  end
end
