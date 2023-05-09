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
    band = create(:band)
    performance = create(:performance, band: band)
    recording = create(:recording, performance: performance)
    create_list(:track, 3, recording: recording)
    assert_equal 3, band.tracks.count
  end

  test "with_metadata" do
    band = create(:band)
    performance = create(:performance, band: band)
    create_list(:recording, 5, performance: performance)
    assert_equal 1, Band.with_metadata.first.num_performances
    assert_equal 5, Band.with_metadata.first.num_recordings
  end
end
