class RecordingsController < ApplicationController
  def show
    # @recording = Recording.find(params[:id])
    # @performance = @recording.performance
    # @band = @recording.band
    #
    # @serialized_playlist = @recording
    #   .playlist
    #   .map { _1.as_json(methods: [:media_url]) }
    #   .index_by { _1["track"] }
    #   .to_json
    render "welcome/index", layout: "application"
  end
end
