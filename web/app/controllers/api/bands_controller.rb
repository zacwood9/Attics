# frozen_string_literal: true

module Api
  class BandsController < ApiController
    def index
      bands = Band.with_metadata.all
      render json: BandBlueprint.render(bands, view: :with_metadata)
    end
  end
end
