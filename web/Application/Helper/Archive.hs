module Application.Helper.Archive
  ( ArchiveItem (..),
    ArchiveFile (..),
    ArchiveSong (..),
    AdvancedSearchSort (..),
    ItemFiles(..),
    getItemFiles,
    scrape,
    advancedSearch,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import IHP.Prelude
import Network.HTTP.Simple

data Source = Single Text | Multi [Text]

data ArchiveItem = ArchiveItem
  { identifier :: Text,
    date :: Text,
    collection :: Maybe Text,
    transferer :: Maybe Text,
    downloads :: Maybe Int,
    source :: Maybe Text,
    avgRating :: Maybe Text,
    numReviews :: Maybe Int,
    lineage :: Maybe Text,
    coverage :: Maybe Text,
    venue :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON ArchiveItem where
  parseJSON = withObject "ArchiveItem" $ \obj -> do
    ArchiveItem
      <$> obj .: "identifier"
      <*> obj .: "date"
      <*> obj .:? "collection"
      <*> obj .:? "transferer"
      <*> obj .:? "downloads"
      <*> parseSource obj
      <*> obj .:? "avg_rating"
      <*> obj .:? "num_reviews"
      <*> obj .:? "lineage"
      <*> obj .:? "coverage"
      <*> obj .:? "venue"

parseSource :: Object -> Parser (Maybe Text)
parseSource o = do
  let source = o HM.! "source"
  case source of
    String single -> pure $ pure single
    Array multiple -> pure $ multiple V.!? 0
      |> \case
        Just (String first) -> pure first
        _ -> Nothing
    _ -> pure Nothing

data ScrapeResponse = ScrapeResponse
  { scrapeItems :: [ArchiveItem],
    scrapeCursor :: Maybe Text
  }
  deriving (Generic)

instance FromJSON ScrapeResponse where
  parseJSON = withObject "ScrapeResponse" $ \obj ->
    ScrapeResponse
      <$> obj .: "items"
      <*> obj .:? "cursor"

scrape :: Text -> IO [ArchiveItem]
scrape t = scrape' t Nothing

scrape' :: Text -> Maybe Text -> IO [ArchiveItem]
scrape' collection cursor =
  let baseUrl = "https://archive.org/services/search/v1/scrape?fields=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier&q=collection:" <> collection
      url = case cursor of
        Just c -> baseUrl <> "&cursor=" <> c
        Nothing -> baseUrl
   in do
        request <- parseRequest (cs url)
        putStrLn url
        response <- httpJSON request
        let ScrapeResponse {..} = getResponseBody response
        case scrapeCursor of
          Just cursor -> do
            rest <- scrape' collection (Just cursor)
            return $ scrapeItems ++ rest
          Nothing -> return scrapeItems

-- FILES

data ArchiveFile = ArchiveFile
  { afFileName :: Text,
    afCreator :: Maybe Text,
    afTitle :: Maybe Text,
    afTrack :: Maybe Text,
    afAlbum :: Maybe Text,
    afLength :: Maybe Text,
    afOriginal :: Maybe Text,
    afFormat :: Maybe Text
  }
  deriving (Generic)

instance FromJSON ArchiveFile where
  parseJSON = withObject "ArchiveFile" $ \obj -> do
    fileName <- obj .: "name"
    creator <- obj .:? "creator"
    title <- obj .:? "title"
    track <- obj .:? "track"
    album <- obj .:? "album"
    length <- obj .:? "length"
    original <- parseOriginal obj
    format <- obj .:? "format"
    return
      ArchiveFile
        { afTitle = title,
          afCreator = creator,
          afFileName = fileName,
          afTrack = track,
          afAlbum = album,
          afLength = length,
          afOriginal = original,
          afFormat = format
        }
    where
      parseOriginal :: Object -> Parser (Maybe Text)
      parseOriginal p = do
        return $
          HM.lookup "original" p
            >>= \case
              String t -> Just t
              _ -> Nothing

newtype ArchiveFilesResponse = ArchiveFilesResponse
  { result :: [ArchiveFile]
  }
  deriving (Generic)

instance FromJSON ArchiveFilesResponse where
  parseJSON = withObject "ArchiveFilesResponse" $ \obj -> do
    f <- obj .: "result"
    files <- parseJSON f
    pure $ ArchiveFilesResponse files

getArchiveFiles :: Text -> IO [ArchiveFile]
getArchiveFiles identifier = do
  request <- parseRequest url
  songs <- httpJSON request
  case getResponseBody songs of
    Just payload -> return $ result payload
    Nothing -> error "unable to parse files"
  where
    url = "https://archive.org/metadata/" ++ T.unpack identifier ++ "/files"

-- Recent sources

advancedSearch :: Text -> AdvancedSearchSort -> IO [(ArchiveItem, UTCTime)]
advancedSearch collection sortBy = do
  request <- parseRequest (cs url)
  response <- httpJSON request
  case getResponseBody response of
    PublicDateResponse pairs -> pure pairs
    ReviewDateResponse pairs -> pure pairs
  where
    baseUrl = "https://archive.org/advancedsearch.php"
    q = "q=collection:%22" <> collection <> "%22"
    sortedField = case sortBy of
      PublicDate -> "publicdate"
      ReviewDate -> "reviewdate"

    fields = "fl%5B%5D=avg_rating,venue,coverage,num_reviews,date,downloads,source,transferer,lineage,identifier," <> sortedField

    sort = "sort%5B%5D=" <> sortedField <> "%20desc"
    output = "output=json"

    buildUrl :: Text -> [Text] -> Text
    buildUrl base fields =
      base <> "?" <> intercalate "&" fields

    url = buildUrl baseUrl [q, fields, sort, output]

data AdvancedSearchSort
  = PublicDate
  | ReviewDate

data AdvancedSearchResult
  = PublicDateResponse [(ArchiveItem, UTCTime)]
  | ReviewDateResponse [(ArchiveItem, UTCTime)]
  deriving (Generic)

instance FromJSON AdvancedSearchResult where
  parseJSON = withObject "AdvancedSearchResponse" $ \obj -> do
    (Object header) <- obj .: "responseHeader"
    (Object params) <- header .: "params"
    (String sort) <- params .: "sort"
    let sortedField = head $ T.words sort

    case sortedField of
      Just "publicdate" -> PublicDateResponse <$> parsePairs obj "publicdate"
      Just "reviewdate" -> ReviewDateResponse <$> parsePairs obj "reviewdate"
      _ -> error "bad AdvancedSearch response"
    where
      parsePairs obj sortedField = do
        (Object response) <- obj .: "response"
        let items@(Array arr) = response HM.! "docs"
        srcs <- parseJSON items
        times <- mapM (\(Object a) -> a .: sortedField) arr
        pure $ zip srcs (V.toList times)

data ItemFiles = ItemFiles
  { mp3s :: [ArchiveSong],
    originals :: [ArchiveSong],
    pic :: Text
  }
  deriving (Generic, Show)

data ArchiveSong = ArchiveSong
  { atticsSongFileName :: Text,
    atticsSongCreator :: Text,
    atticsSongTitle :: Text,
    atticsSongTrack :: Int,
    atticsSongAlbum :: Text,
    atticsSongLength :: Text
  }
  deriving (Generic, Show)

-- | getItemFiles constructs a ItemFiles record for a given recording identifier.
getItemFiles :: Text -> IO ItemFiles
getItemFiles identifier = do
  files <- getArchiveFiles identifier
  return $ buildFiles identifier files

-- | buildFiles constructs a collection of mp3s, originals, and a icon for a given source identifier.
buildFiles :: Text -> [ArchiveFile] -> ItemFiles
buildFiles source archiveFiles =
  ItemFiles
    { mp3s = mapInd toSong mp3s,
      originals = catMaybes $ mapInd maybeToSong originals,
      pic = picUrl
    }
  where
    mp3s =
      filter
        (\ArchiveFile {afFileName} -> ".mp3" `T.isSuffixOf` afFileName)
        archiveFiles

    fileMap =
      foldr
        (\file acc -> HM.insert (afFileName file) file acc)
        HM.empty
        archiveFiles

    originals =
      map
        (afOriginal >=> \f -> HM.lookup f fileMap)
        mp3s

    toSong ArchiveFile {..} i =
      ArchiveSong
        { atticsSongFileName = afFileName,
          atticsSongTitle = fromMaybe afFileName afTitle,
          atticsSongTrack = i + 1,
          atticsSongCreator = fromMaybe "Unknown" afCreator,
          atticsSongLength = fromMaybe "0:00" afLength,
          atticsSongAlbum = fromMaybe "Unknown" afAlbum
        }

    maybeToSong (Just file) i =
      Just $ toSong file i
    maybeToSong Nothing _ = Nothing

    -- variant of map that passes each element's index as a second argument to f
    mapInd :: (a -> Int -> b) -> [a] -> [b]
    mapInd f l = zipWith f l [0 ..]

    picUrl = case filter (\ArchiveFile {afFileName} -> afFileName == "__ia_thumb.jpg") archiveFiles of
      [] -> "https://archive.org/images/notfound.png"
      (file : _) -> "https://archive.org/download/" <> source <> "/" <> afFileName file