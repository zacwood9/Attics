require "test_helper"

module Api
  module Legacy
    class RecordingsControllerTest < ActionDispatch::IntegrationTest
      test "should get show" do
        get api_legacy_performance_recordings_url(
              collection: "GratefulDead",
              date: "1977-05-08",
              format: :json
            )
        assert_response :success
      end

      test "should get show with archive identifier" do
        get api_legacy_performance_recordings_url(
              collection: "GratefulDead",
              date: "1977-05-08",
              format: :json
            )
        assert_response :success
      end
    end
  end
end
