class Admin::BandsController < Admin::AdminController
  before_action :set_band, only: %i[ show edit update destroy ]

  def index
    @bands = Band.order(name: :asc).all
  end

  def show
  end

  def new
    @band = Band.new
  end

  def edit
  end

  def create
    @band = Band.new(band_params)

    if @band.save
      InitialScrapeJob.perform_later(@band.id)
      redirect_to admin_band_url(@band), notice: "Band was successfully created."
    else
      render :new, status: :unprocessable_entity
    end
  end

  def update
    respond_to do |format|
      if @band.update(band_params)
        format.html { redirect_to admin_band_url(@band), notice: "Band was successfully updated." }
      else
        format.html { render :edit, status: :unprocessable_entity }
      end
    end
  end

  def destroy
    @band.destroy

    respond_to do |format|
      format.html { redirect_to admin_bands_url, notice: "Band was successfully destroyed." }
    end
  end

  private

  # Use callbacks to share common setup or constraints between actions.
  def set_band
    @band = Band.find(params[:id])
  end

  # Only allow a list of trusted parameters through.
  def band_params
    params.require(:band).permit(:name, :collection, :logo_url)
  end
end
