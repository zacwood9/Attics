class BandsController < ApplicationController
  def index
    @bands = Band
      .with_metadata
      .order('bands.name asc')
  end
end
