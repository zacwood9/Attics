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
require "test_helper"

class PerformanceTest < ActiveSupport::TestCase
  test "performance" do
    performance = performances(:grateful_dead_1977_05_08)

    Performance.with_recording_metadata.find(performance.id).tap do |p|
      assert_equal performance.recordings.count, p.num_recordings
      assert_equal performance.recordings.sum(:num_reviews), p.num_reviews
      assert_equal 4.9, p.avg_rating
    end
  end

  test "in year" do
    assert_equal 1, Performance.in_year(1977).count
  end

  test "on date" do
    assert_equal 1, Performance.on_day(month: 5, day: 8).count
  end
end
