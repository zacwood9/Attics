module Api
  module Legacy
    class TracksController < ApiController
      def index
        @recording = Recording.find_by!(identifier: params[:identifier])
        @performance = Performance.with_recording_metadata.find(@recording.performance_id)
        @band = @performance.band
        @tracks = @recording.playlist

        respond_to do |format|
          format.html { render 'welcome/index', layout: 'application' }
          format.json
        end
      end
    end
  end
end
