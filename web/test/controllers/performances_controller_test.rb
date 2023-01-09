require "test_helper"

class PerformancesControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get band_performances_url(collection: bands(:grateful_dead).collection, format: :json)
    assert_response :success
  end
end
