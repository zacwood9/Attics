require "internet_archive"
require "async"

Rails.logger = Logger.new(STDOUT)
trackless_recordings = Recording
  .left_joins(:tracks)
  .select("recordings.*, count(tracks.*) as track_count")
  .group(:id)
  .filter { |x| x.track_count.zero? }

trackless_recordings.map do |recording|
  Async do
    files = InternetArchive.files(recording.identifier)
    attributes = Track.attributes_from_files(recording.id, files)
    if attributes.empty?
      recording.destroy!
    else
      Track.insert_all! attributes
    end
  end
end.each(&:wait)
