# frozen_string_literal: true

module Api
  class PerformancesController < ApiController
    def show
      performance = Performance.find(params[:id])
      render json: RecordingBlueprint.render(performance.recordings)
    end
  end
end
