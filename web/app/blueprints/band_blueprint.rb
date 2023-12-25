class BandBlueprint < BaseBlueprint
  identifier :id
  fields :collection, :name, :logo_url, :created_at, :updated_at

  view :with_metadata do
    fields :num_performances, :num_recordings
  end
end
