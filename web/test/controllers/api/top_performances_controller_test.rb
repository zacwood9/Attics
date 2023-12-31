# frozen_string_literal: true

require 'test_helper'

module Api
  class TopPerformancesControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_band_top_performances_path(bands(:grateful_dead))
      assert_response :ok
    end
  end
end
