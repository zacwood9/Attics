module InternetArchive
  class File < Data.define(:name, :title, :track, :length, :creator, :album, :original)
    def playable?
      mp3? || flac?
    end

    def mp3?
      name.end_with?(".mp3")
    end

    def flac?
      name.end_with?(".flac")
    end
  end
end
