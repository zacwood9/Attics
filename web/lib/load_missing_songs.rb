require "rest-client"

def create_songs(recording)
  response = RestClient.get("https://archive.org/metadata/#{recording.identifier}/files")
  result = JSON.parse(response.body)["result"] || []

  ActiveRecord::Base.transaction do
    result = result
      .select { |file| file["name"]&.end_with?(".mp3") }

    return unless result.present?

    pp recording.identifier
    result.map { |file|
        # Sometimes derivative files are missing data that is present
        # on the file's source, so fetch both.
        if file["original"].present?
          [file, result.find { |other| other["name"] == file["original"] } || {}]
        else
          [file, {}]
        end
      }
      .each.with_index { |pair, i|
        file, original = pair
        Track.create!(
          file_name: file["name"],
          title: file["title"] || original["title"],
          track: i + 1,
          length: file["length"] || original["length"],
          creator: file["creator"] || original["creator"],
          album: file["album"] || original["album"],
          recording_id: recording.id,
        )
      }
  end
end

Recording
  .select('recordings.*')
  .select('coalesce(count(tracks.*), 0) as num_tracks')
  .left_joins(:tracks)
  .group('recordings.id')
  .filter { |x| x.num_tracks.zero? }
  .sort_by(&:identifier)
  .each(&method(:create_songs))

