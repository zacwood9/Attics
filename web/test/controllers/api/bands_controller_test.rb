# frozen_string_literal: true

require "test_helper"

module Api
  class BandsControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_bands_path
      assert_response :ok
    end
  end
end
