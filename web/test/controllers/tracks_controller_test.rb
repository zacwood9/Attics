require "test_helper"

class TracksControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get recording_tracks_url(identifier: recordings(:barton_hall_charlie).identifier, format: :json)
    assert_response :success
  end
end
