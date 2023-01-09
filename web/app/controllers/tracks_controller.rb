class TracksController < ApplicationController
  before_action :set_recording, :set_performance, :set_band

  def index
    @tracks = @recording.tracks

    respond_to do |format|
      format.html { render 'welcome/index' }
      format.json
    end
  end

  private

  def set_recording
    @recording ||= Recording.find_by!(identifier: params[:identifier])
  end

  def set_performance
    @performance ||= Performance.with_recording_metadata.find(@recording.performance_id)
  end

  def set_band
    @band ||= @performance.band
  end
end
