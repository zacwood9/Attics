json.band do
  json.id @band.id
  json.collection @band.collection
  json.name @band.name
  json.logo_url @band.logo_url
end

json.performance do
  json.id  @performance.id
  json.date @performance.date
  json.venue @performance.venue.to_s
  json.city @performance.city.to_s
  json.state @performance.state.to_s
  json.num_reviews @performance.num_reviews || 0
  json.num_recordings @performance.num_recordings || 0
  json.avg_rating @performance.avg_rating || 0
  json.band_id @performance.band_id
end

json.recordings do
  json.array! @recordings do |recording|
    json.id recording.id
    json.identifier recording.identifier
    json.transferer recording.transferer.to_s
    json.source recording.source.to_s
    json.avg_rating recording.avg_rating || 0
    json.attics_downloads recording.attics_downloads || 0
    json.archive_downloads recording.archive_downloads || 0
    json.num_reviews recording.num_reviews || 0
    json.lineage recording.lineage.to_s
    json.performance_id recording.performance_id
  end
end
