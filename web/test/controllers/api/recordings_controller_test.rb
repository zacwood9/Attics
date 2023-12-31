# frozen_string_literal: true

require 'test_helper'

module Api
  class RecordingsControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_recording_path(recordings(:grateful_dead_1977_05_08_hicks_sbd))
      assert_response :ok
    end

    def test_get__with_identifier
      get api_recording_path(recordings(:grateful_dead_1977_05_08_hicks_sbd).identifier)
      assert_response :ok
    end
  end
end
