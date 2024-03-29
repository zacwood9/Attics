require "test_helper"

module Api
  module Legacy
    class PerformancesControllerTest < ActionDispatch::IntegrationTest
      test "should get index" do
        get api_legacy_band_performances_url(collection: "GratefulDead", year: "1977", format: :json)
        assert_response :ok
        body = JSON.parse(response.body)
        assert_equal 1, body["performances"].length
      end

      test "top performances should sort by star rating" do
        get api_legacy_band_top_performances_url(
              collection: "GratefulDead",
              format: :json
            )

        assert_response :success
        body = JSON.parse(response.body)

        assert_equal "1977-05-08", body.dig("top_performances", "1977", 0, "date")
        assert_equal "1973-06-10", body.dig("top_performances", "1973", 0, "date")
      end

      test "top performances should include on_this_day" do
        travel_to Date.new(2023, 5, 8)

        get api_legacy_band_top_performances_url(
              collection: "GratefulDead",
              on_this_day: "2022-05-08",
              format: :json
            )

        assert_response :success
        body = JSON.parse(response.body)

        assert_equal "1977-05-08", body["on_this_day"][0]["date"]
      end
    end
  end
end
