module Api
  module Legacy
    class PerformancesController < ApiController
      before_action :set_band

      def index
        @performances = @band.performances.with_recording_metadata

        if (year = params[:year]).present?
          @performances =
            @performances.where("date_part('year', performances.date) = ?", year)
        end

        @performances = @performances.order(date: :asc)
      end

      def top
        @top_performances =
          @band
            .performances
            .with_recording_metadata
            .group_by(&:year)
            .transform_values do |performances|
              performances
                .sort_by(&:avg_rating)
                .reverse
                .first(performances_per_year)
            end
            .sort_by(&:first)
            .to_a

        if (date = params[:on_this_day]&.to_date)
          @on_this_day_performances =
            @band
              .performances
              .with_recording_metadata
              .where("date_part('month', performances.date) = ?", date.month)
              .where("date_part('day', performances.date) = ?", date.day)
              .order(date: :asc)
        else
          @on_this_day_performances = []
        end
      end

      private

      def set_band
        @band = Band.find_by!(collection: params[:collection])
      end

      def performances_per_year
        params[:recordings_per_year]&.to_i || 5
      end
    end
  end
end
