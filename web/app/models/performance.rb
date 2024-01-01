# == Schema Information
#
# Table name: performances
#
#  id         :uuid             not null, primary key
#  city       :string
#  date       :date             not null
#  state      :string
#  venue      :string
#  created_at :datetime         not null
#  updated_at :datetime         not null
#  band_id    :uuid             not null
#
# Indexes
#
#  index_performances_on_band_id  (band_id)
#
# Foreign Keys
#
#  fk_rails_...  (band_id => bands.id)
#
class Performance < ApplicationRecord
  belongs_to :band, optional: false
  has_many :recordings, dependent: :destroy
  has_many :tracks, through: :recordings

  validates :date, presence: true

  attribute :num_recordings, :integer, default: 0
  attribute :num_stars, :float, default: 0
  attribute :num_reviews, :integer, default: 0

  scope :with_recording_metadata, -> {
    select('performances.*')
     .select('count(distinct recordings.id) as num_recordings')
     .select('COALESCE(sum(recordings.avg_rating * recordings.num_reviews), 0) as num_stars')
     .select('COALESCE(sum(recordings.num_reviews), 0) as num_reviews')
     .left_joins(:recordings)
     .order(date: :asc)
     .group('performances.id')
  }

  scope :in_year, ->(year) {
    where("date_part('year', performances.date) = ?", year)
  }

  scope :on_day, ->(month:, day:) {
    where("date_part('month', performances.date) = ?", month)
      .where("date_part('day', performances.date) = ?", day)
      .order(date: :asc)
  }

  def year
    date.year
  end

  def avg_rating
    return 0 if num_reviews.zero?

    (num_stars / num_reviews).round(2)
  end
end
