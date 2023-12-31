# frozen_string_literal: true

module Api
  class RecordingsController < ApiController
    def show
      render json: {
        band: BandBlueprint.render_as_hash(recording.band),
        performance: PerformanceBlueprint.render_as_hash(recording.performance),
        recording: RecordingBlueprint.render_as_hash(recording),
        tracks: TrackBlueprint.render_as_hash(recording.playlist)
      }
    end

    private

    def recording
      @recording ||= begin
        Recording.find(params[:id])
      rescue ActiveRecord::RecordNotFound
        Recording.find_by!(identifier: params[:id])
      end
    end
  end
end
