module InternetArchive
  class File < Data.define(:name, :title, :track, :length, :creator, :album, :original)
    def mp3?
      name.end_with?(".mp3")
    end
  end
end
