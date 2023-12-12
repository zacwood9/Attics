class BandsController < ApplicationController
  def show
    @band = Band.find(params[:id])
  end
end
