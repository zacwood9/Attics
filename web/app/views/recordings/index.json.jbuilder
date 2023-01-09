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

json.recordings do
  json.array! @recordings do |recording|
    json.extract! recording, :id, :identifier, :transferer, :source, :avg_rating, :attics_downloads,
                  :archive_downloads, :num_reviews, :lineage, :performance_id
  end
end
