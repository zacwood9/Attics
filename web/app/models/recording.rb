class Recording < ApplicationRecord
  belongs_to :performance
  has_many :tracks

  validates :identifier, presence: true
end
