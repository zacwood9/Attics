module Api
  class BandsController < ApiController
    def index
      @bands = Band.with_metadata.order(name: :asc)
    end
  end
end
