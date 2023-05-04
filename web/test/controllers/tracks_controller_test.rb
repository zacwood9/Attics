require "test_helper"

class TracksControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    recording = create(:recording)
    get recording_tracks_url(identifier: recording.identifier, format: :json)
    assert_response :success
  end
end
