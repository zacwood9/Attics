# == Schema Information
#
# Table name: tracks
#
#  id           :uuid             not null, primary key
#  album        :string
#  creator      :string
#  file_name    :string           not null
#  length       :string           not null
#  title        :string
#  track        :integer          not null
#  created_at   :datetime         not null
#  updated_at   :datetime         not null
#  recording_id :uuid             not null
#
# Indexes
#
#  index_tracks_on_recording_id  (recording_id)
#
# Foreign Keys
#
#  fk_rails_...  (recording_id => recordings.id)
#
require "test_helper"

class TrackTest < ActiveSupport::TestCase
  test "relationships" do
    track = create(:track)
    assert track.recording
    assert track.performance
    assert track.band
  end
end
