class PerformancesController < ApplicationController
  before_action :set_band

  def index
    set_performances
  end

  def top
    set_top_performances
  end

  private

  def set_band
    @band = Band.find_by!(collection: params[:collection])
  end

  def set_performances
    @performances = @band
      .performances
      .with_recording_metadata

    if (year = params[:year]).present?
      @performances = @performances.where("date_part('year', performances.date) = ?", year)
    end

    @performances = @performances.order(date: :asc)
  end

  def set_top_performances
    @top_performances ||= @band
      .performances
      .with_recording_metadata
      .group_by(&:year)
      .transform_values { |performances|
        performances
          .sort_by(&:avg_rating)
          .reverse
          .first(performances_per_year)
      }
      .sort_by(&:first)
      .to_a
  end

  def performances_per_year
    params[:performances_per_year]&.to_i || 5
  end
end
