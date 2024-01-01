require 'async'
require 'async/semaphore'

namespace :attics do
  desc 'Runs nightly scrape on all bands.'
  task nightly_scrape: :environment do
    logger = Logger.new(STDOUT)
    logger.level = :info
    Rails.logger = logger

    NightlyScrape.new.scrape
  end
end
