# frozen_string_literal: true

module Api
  class YearsController < ApiController
    def show
      band = Band.find(params[:band_id])
      year = Year.new(band:, year: params[:year])
      render json: PerformanceBlueprint.render(year.performances, view: :with_metadata)
    end
  end
end
