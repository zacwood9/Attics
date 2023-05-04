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
    performance = create(:performance)
    create(:recording, performance: performance, avg_rating: 5, num_reviews: 1)
    create(:recording, performance: performance, avg_rating: 4.5, num_reviews: 2)

    assert_equal 0, performance.num_recordings

    Performance.with_recording_metadata.find(performance.id).tap do |p|
      assert_equal 2, p.num_recordings
      assert_equal 3, p.num_reviews
      assert_equal 4.67, p.avg_rating
    end
  end
end
