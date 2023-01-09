require "test_helper"

class RecordingsControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get performance_recordings_url(
      collection: "GratefulDead",
      date: "1977-05-08",
      format: :json
    )
    assert_response :success
  end
end
