module InternetArchive
  class File < BaseStruct
    attribute :name, Types::String
    attribute :title, Types::String.optional
    attribute :track, Types::String.optional
    attribute :length, Types::String.optional
    attribute :creator, Types::String.optional
    attribute :album, Types::String.optional
    attribute :original, Types::String.optional | Types::Array.optional

    def mp3?
      name.end_with?(".mp3")
    end
  end
end
