class Performance < ApplicationRecord
  belongs_to :band, optional: false
  has_many :recordings

  validates :date, presence: true

  scope :with_recording_metadata, -> {
    select('performances.*')
     .select('count(distinct recordings.id) as num_recordings')
     .select('sum(recordings.avg_rating * recordings.num_reviews) as num_stars')
     .select('sum(recordings.num_reviews) as num_reviews')
     .left_joins(:recordings)
     .group('performances.id')
  }

  def year
    date.year
  end

  def avg_rating
    return 0 if num_reviews.zero?

    (num_stars / num_reviews).round(2)
  end
end
