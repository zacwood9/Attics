# == Schema Information
#
# Table name: recordings
#
#  id                :uuid             not null, primary key
#  archive_downloads :integer          default(0), not null
#  attics_downloads  :integer          default(0), not null
#  avg_rating        :float            default(0.0), not null
#  identifier        :string           not null
#  lineage           :string
#  num_reviews       :integer          default(0), not null
#  source            :string
#  transferer        :string
#  created_at        :datetime         not null
#  updated_at        :datetime         not null
#  performance_id    :uuid             not null
#
# Indexes
#
#  index_recordings_on_performance_id  (performance_id)
#
# Foreign Keys
#
#  fk_rails_...  (performance_id => performances.id)
#
require "test_helper"

class RecordingTest < ActiveSupport::TestCase
  test "relationships" do
    recording = recordings(:grateful_dead_1977_05_08_hicks_sbd)
    assert recording.performance
    assert recording.band
    assert recording.tracks
  end
end
