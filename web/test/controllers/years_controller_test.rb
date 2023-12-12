require "test_helper"

class YearsControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get band_year_url(bands(:grateful_dead), 1977)
    assert_response :success
  end
end
