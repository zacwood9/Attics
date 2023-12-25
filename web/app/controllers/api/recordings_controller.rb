# frozen_string_literal: true

module Api
  class RecordingsController < ApiController
    def show
      recording = Recording.find(params[:id])
      render json: {
        band: BandBlueprint.render_as_hash(recording.band),
        performance: PerformanceBlueprint.render_as_hash(recording.performance),
        recording: RecordingBlueprint.render_as_hash(recording),
        tracks: TrackBlueprint.render_as_hash(recording.playlist)
      }
    end
  end
end
