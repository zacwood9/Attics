require "rest-client"

module InternetArchive
  autoload :File, "internet_archive/file"

  class << self
    def files(identifier)
      response =
        RestClient.get("https://archive.org/metadata/#{identifier}/files")
      result = JSON.parse(response.body)["result"] || []
      result.map do |item|
        InternetArchive::File.new(
          name: item["name"],
          title: item["title"],
          track: item["track"],
          length: item["length"],
          creator: item["creator"],
          album: item["album"],
          original: item["original"]
        )
      end
    end
  end
end
