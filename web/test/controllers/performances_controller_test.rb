require "test_helper"

class PerformancesControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get performance_path(performances(:grateful_dead_1977_05_08))
    assert_response :success
  end
end
