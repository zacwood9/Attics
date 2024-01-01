class YearsController < ApplicationController
  def show
    # @band = Band.find(params[:band_id])
    # @year = Year.new(band: @band, year: params[:year].to_i)

    render "welcome/index", layout: "application"
  end
end
