# frozen_string_literal: true

require 'test_helper'

module Api
  class TopPerformancesControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_band_top_performances_path(bands(:grateful_dead), on_this_day: "2022-05-08")
      assert_response :ok

      body = JSON.parse(@response.body)
      assert_equal 1, body["on_this_day"].count
    end
  end
end
