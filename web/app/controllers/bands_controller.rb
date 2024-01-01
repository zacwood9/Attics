class BandsController < ApplicationController
  def show
    # @band = Band.find(params[:id])

    render "welcome/index", layout: "application"
  end
end
