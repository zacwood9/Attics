require 'async/semaphore'

class Band::Pruner
  def initialize(band, concurrent_requests: 5)
    @band = band
    @concurrent_requests = concurrent_requests
  end

  def prune
    task.wait
  end

  private

  def task
    Async do
      band.recordings.find_each do |recording|
        semaphore.async do
          recording.prune
        end
      end
    end
  end

  attr_reader :band, :concurrent_requests

  def semaphore
    @semaphore ||= Async::Semaphore.new(concurrent_requests)
  end
end
