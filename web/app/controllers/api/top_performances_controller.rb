# frozen_string_literal: true

module Api
  class TopPerformancesController < ApiController
    def index
      band = Band.find(params[:band_id])
      per_year = params[:performances_per_year]&.to_i || 5
      render json: band.years.map { |year|
        {
          year: year.year.to_s,
          top_performances: PerformanceBlueprint.render_as_hash(year.top_performances(per_year), view: :with_metadata)
        }
      }
    end
  end
end
