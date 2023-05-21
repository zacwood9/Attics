class ScrapeJob < ApplicationJob
  queue_as :default

  def perform(band)
    Band::ArchiveScraper.new(band).scrape
  end
end
