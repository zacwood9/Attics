module Band::Scrapeable
  def scrape
    ScrapeJob.perform_later self
  end
end
