module Api
  class RecordingsController < ApiController
    def index
      @band = Band.find_by!(collection: params[:collection])
      @performance = Performance.with_recording_metadata.find_by!(band: @band, date: params[:date])
      @recordings = @performance.recordings
    end
  end
end
