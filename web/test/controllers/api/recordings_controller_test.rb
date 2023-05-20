require "test_helper"

module Api
  class RecordingsControllerTest < ActionDispatch::IntegrationTest
    test "should get index" do
      create(:recording)

      get api_performance_recordings_url(
        collection: "GratefulDead",
        date: "1977-05-08",
        format: :json
      )
      assert_response :success
    end
  end
end
