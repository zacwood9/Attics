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
  test "relationships" do
    assert_equal 4, bands(:grateful_dead).tracks.count
  end

  test "with_metadata" do
    assert_equal 2, Band.with_metadata.find_by(name: 'Grateful Dead').num_performances
  end
end
