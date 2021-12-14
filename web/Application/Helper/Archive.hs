module Application.Helper.Archive
  ( ArchiveItem (..),
    ArchiveFile (..),
    ArchiveSong (..),
    AdvancedSearchSort (..),
    AdvancedSearchResult(..),
    ItemFiles(..),
    scrape,
    advancedSearch,
    getSongsForRecording,
    getArchiveFiles,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import GHC.Generics
import IHP.Prelude
import Network.HTTP.Simple
import System.IO.Unsafe
import Control.Exception
import Data.Default
import Data.Maybe
import Generated.Types
import IHP.ModelSupport

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
      <*> parseAvgRating obj
      <*> obj .:? "num_reviews"
      <*> obj .:? "lineage"
      <*> obj .:? "coverage"
      <*> obj .:? "venue"

instance Default ArchiveItem where
    def = ArchiveItem "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parseSource :: Object -> Parser (Maybe Text)
parseSource o = do
  let source = o HashMap.!? "source"
  case source of
    Just (String single) -> pure $ pure single
    Just (Array multiple) -> pure $ multiple Vector.!? 0
      |> \case
        Just (String first) -> pure first
        _ -> Nothing
    _ -> pure Nothing

parseAvgRating :: Object -> Parser (Maybe Text)
parseAvgRating o = do
  let source = o HashMap.!? "avg_rating"
  case source of
    Just (Number num) -> pure $ Just (tshow num)
    Just (String text) -> pure $ Just text
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
          HashMap.lookup "original" p
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
    url = "https://archive.org/metadata/" ++ Text.unpack identifier ++ "/files"

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
  deriving (Generic, Show)

instance FromJSON AdvancedSearchResult where
  parseJSON = withObject "AdvancedSearchResponse" $ \obj -> do
    (Object header) <- obj .: "responseHeader"
    (Object params) <- header .: "params"
    (String sort) <- params .: "sort"
    let sortedField = head $ Text.words sort
    case sortedField of
      Just "publicdate" -> PublicDateResponse <$> parsePairs obj "publicdate"
      Just "reviewdate" -> ReviewDateResponse <$> parsePairs obj "reviewdate"
      _ -> error "bad AdvancedSearch response"
    where
      parsePairs obj sortedField = do
        (Object response) <- obj .: "response"
        let items@(Array arr) = response HashMap.! "docs"
        srcs <- parseJSON items
        times <- catMaybes . Vector.toList <$> mapM (\(Object a) -> a .:? sortedField) arr
        if List.length times /= List.length arr
          then pure []
          else pure $ zip srcs times

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

getSongsForRecording :: Recording -> (Text -> IO [ArchiveFile]) -> IO [Song]
getSongsForRecording recording getFiles = do
    files <- getFiles (get #identifier recording)
    pure $ buildSongsFromFiles recording files

buildSongsFromFiles :: Recording -> [ArchiveFile] -> [Song]
buildSongsFromFiles recording files =
    filesToSongs files
        |> map (makeSongRecord recording)

makeSongRecord :: Recording -> ArchiveSong -> Song
makeSongRecord recording ArchiveSong {..} =
  newRecord @Song
    |> set #title atticsSongTitle
    |> set #album atticsSongAlbum
    |> set #creator atticsSongCreator
    |> set #length atticsSongLength
    |> set #track atticsSongTrack
    |> set #fileName atticsSongFileName
    |> set #recordingId (get #id recording)

-- | Build a list of songs from files, using data from
-- originals if possible.
filesToSongs :: [ArchiveFile] -> [ArchiveSong]
filesToSongs files =
    let
        mp3s = filter
            (\ArchiveFile {afFileName} -> ".mp3" `Text.isSuffixOf` afFileName)
            files

        mp3sPairedWithOriginals = map
            (\mp3 -> (mp3, afOriginal mp3 >>= \f -> HashMap.lookup f fileMap))
            mp3s
    in

        mapInd toSong mp3sPairedWithOriginals

    where
        -- | Hash Map of all files by file name
        fileMap =
            foldr
                (\file acc -> HashMap.insert (afFileName file) file acc)
                HashMap.empty
                files

        toSong (mp3, original) i =
            ArchiveSong {
                atticsSongFileName = afFileName mp3,
                atticsSongTitle = pluckFromPair mp3 original afFileName afTitle,
                atticsSongTrack = i + 1,
                atticsSongCreator = pluckFromPair mp3 original (const "Unknown") afCreator,
                atticsSongLength = fromMaybe "0:00" $ afLength mp3,
                atticsSongAlbum = pluckFromPair mp3 original (const "Unknown") afAlbum
                }

        mapInd :: (a -> Int -> b) -> [a] -> [b]
        mapInd f l = zipWith f l [0 ..]

pluckFromPair
    :: ArchiveFile
    -> Maybe ArchiveFile
    -> (ArchiveFile -> a)
    -> (ArchiveFile -> Maybe a)
    -> a
pluckFromPair mp3 mbOriginal getDefault getPreferred =
    case mbOriginal of
        Just original -> fromMaybe
            (getDefault mp3)
            (firstJust [getPreferred original, getPreferred mp3])
        Nothing -> fromMaybe (getDefault mp3) (getPreferred mp3)

firstJust :: [Maybe a] -> Maybe a
firstJust xs = case filter isJust xs of
    [] -> Nothing
    (a:_) -> a
