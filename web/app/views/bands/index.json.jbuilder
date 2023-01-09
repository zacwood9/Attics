json.array! @bands do |band|
  json.extract! band,
                :id,
                :collection,
                :name,
                :logo_url,
                :num_performances,
                :num_recordings,
                :created_at,
                :updated_at
end
