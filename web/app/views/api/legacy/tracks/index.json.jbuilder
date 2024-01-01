json.band { json.partial! @band }
json.performance { json.partial! @performance }
json.recording { json.partial! @recording }
json.songs do
  json.partial! "api/legacy/tracks/track", collection: @tracks, as: :track
end
