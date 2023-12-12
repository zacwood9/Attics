# == Schema Information
#
# Table name: tracks
#
#  id           :uuid             not null, primary key
#  album        :string
#  creator      :string
#  file_name    :string           not null
#  length       :string           not null
#  title        :string
#  track        :integer          not null
#  created_at   :datetime         not null
#  updated_at   :datetime         not null
#  recording_id :uuid             not null
#
# Indexes
#
#  index_tracks_on_recording_id  (recording_id)
#
# Foreign Keys
#
#  fk_rails_...  (recording_id => recordings.id)
#
class Track < ApplicationRecord
  belongs_to :recording
  has_one :performance, through: :recording
  has_one :band, through: :performance

  validates :file_name, presence: true
  validates :track, presence: true
  validates :length, presence: true

  def mp3?
    file_name.ends_with? ".mp3"
  end

  def flac?
    file_name.ends_with? ".flac"
  end

  def media_url
    "https://archive.org/download/#{recording.identifier}/#{file_name}"
  end

  def self.attributes_from_files(recording_id, archive_files)
    archive_files
      .filter(&:playable?)
      .map { |file|
        # Sometimes derivative files are missing data that is present
        # on the file's source, so fetch both.
        if file.original.present?
          [file, archive_files.find { |other| other.name == file.original }]
        else
          [file, nil]
        end
      }
      .map.with_index { |(file, original), i|
        length = (file.length || original&.length).then do |len|
          if /[0-9]*\.[0-9]/.match? len
            convert_to_minutes_seconds len
          else
            len
          end
        end

        {
          file_name: file.name,
          title: file.title || original&.title,
          track: (file.track.presence || original&.track&.presence)&.to_i || i + 1,
          length: length || "0:00",
          creator: file.creator || original&.creator,
          album: file.album || original&.album,
          recording_id: recording_id,
        }
      }
  end

  def self.convert_to_minutes_seconds(length_in_seconds)
    seconds = length_in_seconds.to_f
    minutes = (seconds / 60).to_i
    seconds = (seconds % 60).round(2)
    "#{'%02d' % minutes}:#{'%02d' % seconds}"
  end
end
