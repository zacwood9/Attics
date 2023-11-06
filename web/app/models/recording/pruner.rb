require 'rest-client'

class Recording::Pruner
  def initialize(recording)
    @recording = recording
  end

  def prune
    return if exists_in_archive?(recording.identifier)

    Rails.logger.info "destroying #{recording.identifier}"
  end

  private

  attr_reader :recording

  def exists_in_archive?(identifier)
    archive_search(identifier)["count"] > 0
  end

  def archive_search(identifier)
    response = RestClient.get(identifier_url(identifier))
    JSON.parse(response.body).tap { |a| Rails.logger.info a.to_s }
  end

  def identifier_url(identifier)
    "https://archive.org/services/search/v1/scrape?fields=identifier&q=identifier:#{identifier}"
  end
end
