require "test_helper"

module Api
  module Legacy
    class BandsControllerTest < ActionDispatch::IntegrationTest
      test "should get index" do
        travel_to Date.new(2022, 3, 20)

        get api_legacy_bands_url(format: :json)
        assert_response :success

        body = JSON.parse(response.body).map(&:deep_symbolize_keys)
        assert_equal bands(:billy_strings).id, body.dig(0, :id)
        assert_equal "BillyStrings", body.dig(0, :collection)
        assert_equal "Billy Strings", body.dig(0, :name)
        assert_equal "https://google.com", body.dig(0, :logo_url)
        assert_equal 0, body.dig(0, :num_performances)
        assert_equal 0, body.dig(0, :num_recordings)

        assert_equal bands(:grateful_dead).id, body.dig(1, :id)
        assert_equal "GratefulDead", body.dig(1, :collection)
        assert_equal "Grateful Dead", body.dig(1, :name)
        assert_equal "https://google.com", body.dig(1, :logo_url)
        assert_equal 2, body.dig(1, :num_performances)
        assert_equal 1, body.dig(1, :num_recordings)
      end
    end
  end
end
