class Track < ApplicationRecord
  belongs_to :recording

  validates :file_name, presence: true
  validates :track, presence: true
  validates :length, presence: true
end
