module Band::Scrapeable
  def scrape(...)
    Band::ArchiveScraper.new(self, ...).scrape
  end

  def scrape_later
    ScrapeJob.perform_later self
  end
end
