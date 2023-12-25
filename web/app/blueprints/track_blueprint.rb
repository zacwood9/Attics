class TrackBlueprint < BaseBlueprint
  identifier :id
  fields :file_name, :length, :track, :recording_id, :created_at, :updated_at

  field :title do |track|
    track.title.presence || track.file_name.presence || "Track #{track.track}"
  end

  field :album do |track|
    track.album.presence || "#{track.performance.date}: #{track.performance.venue}"
  end

  field :artist do |track|
    track.band.name
  end

  field :band_id do |track|
    track.band.id
  end

  field :performance_id do |track|
    track.performance.id
  end

  field :download_url do |track|
    "https://archive.org/download/#{track.recording.identifier}/#{track.file_name}"
  end
end
