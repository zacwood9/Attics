class PerformancesController < ApplicationController
  def show
    @performance = Performance.find(params[:id])
  end
end
