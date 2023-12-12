require "test_helper"

class PerformancesControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get performances_show_url
    assert_response :success
  end
end
