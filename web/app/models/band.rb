class Band < ApplicationRecord
  validates :name, presence: true
  validates :collection, presence: true, uniqueness: true

  has_many :performances

  scope :with_metadata, -> {
    select('bands.*')
     .select('count(distinct performances.id) as num_performances')
     .select('count(distinct recordings.id) as num_recordings')
     .left_joins(performances: [:recordings])
     .group('bands.id')
  }
end
