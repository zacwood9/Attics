module Band::Scrapeable
  def scrape
    Band::ArchiveScraper.new(band).scrape
  end

  def scrape_later
    ScrapeJob.perform_later self
  end
end
