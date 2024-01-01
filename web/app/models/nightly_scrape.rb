# frozen_string_literal: true

class NightlyScrape
  def scrape
    Band.find_each do |band|
      band.scrape
    end
  end
end
