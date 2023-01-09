json.band do
  json.extract! @band,
                :id,
                :collection,
                :name,
                :logo_url
end

json.performance do
  json.extract! @performance, :id, :date, :venue, :city, :state, :num_reviews, :num_recordings, :avg_rating, :band_id
end

json.recording do
  json.extract! @recording, :id, :identifier, :transferer, :source, :avg_rating, :attics_downloads,
                :archive_downloads, :num_reviews, :lineage, :performance_id
end

json.songs do
  json.array! @tracks do |track|
    json.extract! track, :id, :title, :file_name, :album, :track, :length, :recording_id
  end
end