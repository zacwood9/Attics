require 'internet_archive'

recordings = Track.includes(:recording).where(title: nil).map { |x| x.recording }.uniq
pp recordings.map(&:identifier)

recordings.each do |recording|
  files = InternetArchive.files(recording.identifier)
  attributes = Track.attributes_from_files(recording.id, files)

  attributes.zip(recording.tracks.order(track: :asc).to_a).each do |attrs, track|
    pp "#{attrs[:title]}, #{track.title}"
    track.update!(**attrs)
  end
end
