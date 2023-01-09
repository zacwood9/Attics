require "rest-client"
require "json"

# return if Band.any?
#
# File.open('bands.json') do |file|
#   bands = JSON.parse(file.read)
#
#   attrs = bands.map do |band|
#     {
#       id: band["id"],
#       collection: band["collection"],
#       name: band["name"],
#       logo_url: band["logo_url"]
#     }
#   end
#
#   Band.insert_all!(attrs)
# end

# ActiveRecord::Base.transaction do
#   File.open('performances.json') do |file|
#     performances = JSON.parse(file.read)
#     attrs = performances.map do |performance|
#       if performance["date"].nil? || performance['id'] == '7ba03a47-6613-417b-bd65-3baf126212ab'
#         pp performance
#         next
#       end
#
#       {
#         id: performance["id"],
#         date: performance["date"],
#         venue: performance["venue"],
#         city: performance["city"],
#         state: performance["state"],
#         band_id: performance["band_id"],
#       }
#     end
#     Performance.insert_all!(attrs.compact)
#   end
# end

bad_recording = nil
ActiveRecord::Base.transaction do
  File.open('recordings.json') do |file|
    data = JSON.parse(file.read)
    data.each_slice(1000).with_index do |items, i|
      attrs = items.map do |item|
        if item['performance_id'] == '7ba03a47-6613-417b-bd65-3baf126212ab'
          bad_recording = item["id"]
          next
        end

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
end

ActiveRecord::Base.transaction do
  File.open('songs.json') do |file|
    data = JSON.parse(file.read)
    data.each_slice(1000).with_index do |items, i|
      attrs = items.map do |item|
        if item['file_name'].blank? || item['recording_id'] == bad_recording
          next
        end

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
      Song.insert_all!(attrs.compact)
      pp "batch #{i} done"
    end
  end
end


