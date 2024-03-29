class Year
  attr_reader :band, :year

  def initialize(band:, year:)
    @band = band
    @year = year
  end

  def to_param
    year
  end

  def top_performances(number = 5)
    performances.sort_by(&:num_stars).reverse[...number]
  end

  def performances
    @performances ||= band
      .performances
      .with_recording_metadata
      .in_year(year.to_i)
  end
end
