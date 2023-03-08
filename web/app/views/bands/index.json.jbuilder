json.array! @bands do |band|
  json.id band.id
  json.collection band.collection
  json.name band.name
  json.logo_url band.logo_url
  json.num_performances band.num_performances
  json.num_recordings band.num_recordings
  json.created_at band.created_at
  json.updated_at band.updated_at
end
