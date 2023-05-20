require "test_helper"

module Api
  class TracksControllerTest < ActionDispatch::IntegrationTest
    test "should get index" do
      recording = create(:recording)
      get api_recording_tracks_url(identifier: recording.identifier, format: :json)
      assert_response :success
    end
  end
end
