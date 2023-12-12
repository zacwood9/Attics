require "test_helper"

class BandsControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get band_url(bands(:grateful_dead))
    assert_response :success
  end
end
