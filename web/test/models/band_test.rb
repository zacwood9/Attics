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
require "test_helper"

class BandTest < ActiveSupport::TestCase
  # test "the truth" do
  #   assert true
  # end
end
