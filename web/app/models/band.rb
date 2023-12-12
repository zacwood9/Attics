# == Schema Information
#
# Table name: bands
#
#  id         :uuid             not null, primary key
#  collection :string           not null
#  logo_url   :string
#  name       :string           not null
#  url        :string
#  created_at :datetime         not null
#  updated_at :datetime         not null
#
class Band < ApplicationRecord
  include Scrapeable, Prunable

  validates :name, presence: true
  validates :collection, presence: true, uniqueness: true

  has_many :performances, dependent: :destroy
  has_many :recordings, through: :performances
  has_many :tracks, through: :recordings

  attribute :num_performances, :integer, default: 0
  attribute :num_recordings, :integer, default: 0

  scope :with_metadata, -> {
    select('bands.*')
     .select('count(distinct performances.id) as num_performances')
     .select('count(distinct recordings.id) as num_recordings')
     .left_joins(performances: :recordings)
     .group('bands.id')
  }

  def years
    performances.group_by { _1.date.year }.keys.sort.map do |year|
      Year.new(band: self, year: year)
    end
  end
end
