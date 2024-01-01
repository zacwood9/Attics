# frozen_string_literal: true

require "test_helper"

module Api
  class YearsControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_band_year_path(bands(:grateful_dead), 1977)
      assert_response :ok
    end
  end
end
