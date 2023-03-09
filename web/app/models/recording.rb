class Recording < ApplicationRecord
  belongs_to :performance
  has_many :tracks, dependent: :destroy

  validates :identifier, presence: true
end
