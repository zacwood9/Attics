# == Schema Information
#
# Table name: recordings
#
#  id                :uuid             not null, primary key
#  archive_downloads :integer          default(0), not null
#  attics_downloads  :integer          default(0), not null
#  avg_rating        :float            default(0.0), not null
#  identifier        :string           not null
#  lineage           :string
#  num_reviews       :integer          default(0), not null
#  source            :string
#  transferer        :string
#  created_at        :datetime         not null
#  updated_at        :datetime         not null
#  performance_id    :uuid             not null
#
# Indexes
#
#  index_recordings_on_performance_id  (performance_id)
#
# Foreign Keys
#
#  fk_rails_...  (performance_id => performances.id)
#
class Recording < ApplicationRecord
  include Prunable

  belongs_to :performance
  has_one :band, through: :performance

  has_many :tracks, -> { order(track: :asc) }, dependent: :destroy

  validates :identifier, presence: true

  def playlist
    tracks.group_by(&:track).map do |_, tracks|
      if tracks.length == 1
        tracks.first
      else
        tracks.find(&:mp3?) || tracks.first
      end
    end
  end

  def stars
    num_reviews * avg_rating
  end
end
