class Track < ApplicationRecord
  belongs_to :recording

  validates :file_name, presence: true
  validates :track, presence: true
  validates :length, presence: true

  def self.attributes_from_files(recording_id, archive_files)
    archive_files
      .filter(&:mp3?)
      .map { |file|
        # Sometimes derivative files are missing data that is present
        # on the file's source, so fetch both.
        if file.original.present?
          [file, archive_files.find { |other| other.name == file.original }]
        else
          [file, nil]
        end
      }
      .map.with_index { |pair, i|
        file, original = pair
        {
          file_name: file.name,
          title: file.title || original&.title,
          track: i + 1,
          length: file.length || original&.length,
          creator: file.creator || original&.creator,
          album: file.album || original&.album,
          recording_id: recording_id,
        }
      }
  end
end
