# frozen_string_literal: true

require "test_helper"

module Api
  class PerformancesControllerTest < ActionDispatch::IntegrationTest
    def test_get
      get api_performance_path(performances(:grateful_dead_1977_05_08))
      assert_response :ok
    end
  end
end
