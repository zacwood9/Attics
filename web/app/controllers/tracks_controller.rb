class TracksController < ApplicationController
  def index
    @recording ||= Recording.find_by!(identifier: params[:identifier])
    @performance ||= Performance.with_recording_metadata.find(@recording.performance_id)
    @band ||= @performance.band
    @tracks = @recording.tracks

    respond_to do |format|
      format.html { render 'welcome/index' }
      format.json
    end
  end
end
