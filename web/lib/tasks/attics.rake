namespace :attics do
  desc 'Runs nightly scrape on all bands.'
  task nightly_scrape: :environment do
    Band.pluck(:id).each do |band_id|
      NightlyScrapeJob.perform_now(band_id)
    end
  end
end
