module Application.Helper.Scrape
  (
    RecordingData (..),
    archiveToAttics,
    buildPerformanceFromRecordings,
    makeRecordingRecord,
    makeRecordingRecord',
    makeSongRecord,
  )
where

import Application.Helper.Archive
import Application.Script.Prelude
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Functor ((<&>))
import qualified Data.List as List (length)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.SqlQQ
import Text.Read (readMaybe)

-- data PerformanceData = PerformanceData
--   { showCollection :: Text,
--     showDate :: Text,
--     showVenue :: Text,
--     showCity :: Text,
--     showState :: Text
--   }
--   deriving (Show)

data RecordingData = RecordingData
  { identifier :: Text,
    collection :: Text,
    date :: Text,
    venue :: Text,
    city :: Text,
    state :: Text,
    transferer :: Text,
    source :: Text,
    lineage :: Text,
    downloads :: Int,
    avgRating :: Double,
    numReviews :: Int,
    atticsDownloads :: Int
  }
  deriving (Show)

archiveToAttics :: Text -> ArchiveItem -> RecordingData
archiveToAttics defaultCollection ArchiveItem {..} =
  RecordingData
    { collection = fromMaybe defaultCollection collection,
      identifier = identifier,
      date = Text.takeWhile (/= 'T') date,
      transferer = mbUnknown transferer,
      downloads = fromMaybe 0 downloads,
      source = mbUnknown source,
      avgRating = fromMaybe 0 (avgRating >>= readMaybe . cs),
      numReviews = fromMaybe 0 numReviews,
      lineage = mbUnknown lineage,
      venue = mbUnknown venue,
      city = city,
      state = state,
      atticsDownloads = 0
    }
  where
    mbUnknown = fromMaybe "Unknown"
    (city, state) = fromMaybe ("Unknown", "Unknown") $ do
      str <- coverage
      let s = Text.splitOn "," str
      if List.length s == 2
        then pure (mbUnknown $ head s, Text.strip (s !! 1))
        else Nothing

-- data Builder = Builder Band Text Text Text Text [RecordingData]
data Builder = Builder {
  band :: Band,
  date :: Text,
  venue :: Text,
  city :: Text,
  state :: Text,
  recordings :: [RecordingData]
}

buildPerformanceFromRecordings :: Band -> [RecordingData] -> Maybe Performance
buildPerformanceFromRecordings _ [] = Nothing
buildPerformanceFromRecordings band (firstRecording : recordings) =
  let firstState =
        Builder
          band
          (get #date firstRecording)
          (get #venue firstRecording)
          (get #city firstRecording)
          (get #state firstRecording)
          recordings
   in pure $ State.evalState (helper firstRecording) firstState
  where
    helper :: RecordingData -> State Builder Performance
    helper firstSrc = do
      Builder {..} <- State.get
      case recordings of
        [] ->
          pure $
            newRecord @Performance
              |> set #date date
              |> set #venue venue
              |> set #city city
              |> set #state state
              |> set #bandId (get #id band)
        (recording : rest) ->
          let nextVenue = if venue == "Unknown" then get #venue recording else venue
              nextCity = if city == "Unknown" then get #city recording else city
              nextState = if state == "Unknown" then get #state recording else state
           in do
                State.put (Builder band date nextVenue nextCity nextState rest)
                helper firstSrc

makeRecordingRecord performanceId RecordingData {..} =
  newRecord @Recording
    |> set #identifier identifier
    |> set #performanceId performanceId
    |> set #transferer transferer
    |> set #source source
    |> set #lineage lineage
    |> set #archiveDownloads downloads
    |> set #avgRating avgRating
    |> set #numReviews numReviews

makeRecordingRecord' :: (?modelContext :: ModelContext) => Band -> RecordingData -> IO Recording
makeRecordingRecord' band recording = do
  performanceId <- sqlQuery performanceIdQuery [get #identifier recording, get #date recording]

  -- if the performance is found, return its ID,
  -- else create the performance from the recording data and use the new id
  id <- case performanceId of
    (Only id : _) -> pure id
    _ -> case buildPerformanceFromRecordings band [recording] of
      Nothing -> error "unable to build performance from recordings"
      Just performance -> do
        performance
          |> create
          <&> get #id

  pure $ makeRecordingRecord id recording
  where
    performanceIdQuery =
      [sql|
        select
          performances.id from performances
          inner join bands on performances.band_id = bands.id
          where bands.collection = ? and performances.date = ?
      |]

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