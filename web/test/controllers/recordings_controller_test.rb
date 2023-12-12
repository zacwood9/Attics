require "test_helper"

class RecordingsControllerTest < ActionDispatch::IntegrationTest
  test "should get show" do
    get recording_url(recordings(:grateful_dead_1977_05_08_hicks_sbd))
    assert_response :success
  end
end
