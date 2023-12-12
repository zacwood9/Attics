require "test_helper"

class YearsControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get years_show_url
    assert_response :success
  end
end
