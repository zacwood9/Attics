namespace :attics do
  desc 'Runs nightly scrape on all bands.'
  task nightly_scrape: :environment do
    logger = Logger.new(STDOUT)
    logger.level = :info
    Rails.logger = logger

    Band.find_each do |band|
      ScrapeJob.perform_now band
    end
  end
end
