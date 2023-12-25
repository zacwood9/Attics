# frozen_string_literal: true

module Api
  class TopPerformancesController < ApiController
    def index
      band = Band.find(params[:band_id])
      render json: band.years.map { |year|
        {
          year: year.year.to_s,
          top_performances: PerformanceBlueprint.render_as_hash(year.top_performances, view: :with_metadata)
        }
      }
    end
  end
end
