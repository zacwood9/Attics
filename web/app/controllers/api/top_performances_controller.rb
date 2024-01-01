# frozen_string_literal: true

module Api
  class TopPerformancesController < ApiController
    def index
      per_year = params[:performances_per_year]&.to_i || 5

      render json: {
        on_this_day: on_this_day_performances,
        years: band.years.map { |year|
          {
            year: year.year.to_s,
            top_performances: PerformanceBlueprint.render_as_hash(
              year.top_performances(per_year),
              view: :with_metadata
            )
          }
        }
      }
    end

    private

    def band
      @band ||= Band.find(params[:band_id])
    end

    def on_this_day_performances
      return [] if on_this_day.nil?

      performances = band
        .performances
        .with_recording_metadata
        .on_day(month: on_this_day.month, day: on_this_day.day).order(date: :asc)

      PerformanceBlueprint.render_as_hash(performances, view: :with_metadata)
    end

    def on_this_day
      params[:on_this_day]&.to_date
    end
  end
end
