require "test_helper"

module Api
  class TracksControllerTest < ActionDispatch::IntegrationTest
    test "should get index" do
      get api_recording_tracks_url(identifier: recordings(:grateful_dead_1977_05_08_hicks_sbd).identifier, format: :json)
      assert_response :success
    end
  end
end
