require 'async'
require 'async/semaphore'

namespace :attics do
  desc 'Runs nightly scrape on all bands.'
  task nightly_scrape: :environment do
    logger = Logger.new(STDOUT)
    logger.level = :info
    Rails.logger = logger

    Async do
      semaphore = Async::Semaphore.new(2)

      Band.find_each do |band|
        semaphore.async { ScrapeJob.perform_now band }
      end
    end.wait
  end
end
