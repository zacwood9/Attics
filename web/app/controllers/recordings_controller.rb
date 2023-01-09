class RecordingsController < ApplicationController
  before_action :set_band, :set_performance

  def index
    @recordings = @performance.recordings
  end

  private

  def set_band
    @band ||= Band.find_by!(collection: params[:collection])
  end

  def set_performance
    @performance ||= Performance.with_recording_metadata.find_by!(band: @band, date: params[:date])
  end
end
