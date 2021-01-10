module Application.Helper.Scrape
  ( PerformanceData (..),
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

data PerformanceData = PerformanceData
  { showCollection :: Text,
    showDate :: Text,
    showVenue :: Text,
    showCity :: Text,
    showState :: Text
  }
  deriving (Show)

data RecordingData = RecordingData
  { srcIdentifier :: Text,
    srcCollection :: Text,
    srcDate :: Text,
    srcVenue :: Text,
    srcCity :: Text,
    srcState :: Text,
    srcTransferer :: Text,
    srcSource :: Text,
    srcLineage :: Text,
    srcDownloads :: Int,
    srcAvgRating :: Double,
    srcNumReviews :: Int,
    srcAtticsDownloads :: Int
  }
  deriving (Show)

archiveToAttics :: Text -> ArchiveItem -> RecordingData
archiveToAttics defaultCollection ArchiveItem {..} =
  RecordingData
    { srcCollection = fromMaybe defaultCollection collection,
      srcIdentifier = identifier,
      srcDate = Text.takeWhile (/= 'T') date,
      srcTransferer = mbUnknown transferer,
      srcDownloads = fromMaybe 0 downloads,
      srcSource = mbUnknown source,
      srcAvgRating = fromMaybe 0 (avgRating >>= readMaybe . cs),
      srcNumReviews = fromMaybe 0 numReviews,
      srcLineage = mbUnknown lineage,
      srcVenue = mbUnknown venue,
      srcCity = city,
      srcState = state,
      srcAtticsDownloads = 0
    }
  where
    mbUnknown = fromMaybe "Unknown"
    (city, state) = fromMaybe ("Unknown", "Unknown") $ do
      str <- coverage
      let s = Text.splitOn "," str
      if List.length s == 2
        then pure (mbUnknown $ head s, Text.strip (s !! 1))
        else Nothing

data Builder = Builder Band Text Text Text Text [RecordingData]

buildPerformanceFromRecordings :: Band -> [RecordingData] -> Maybe Performance
buildPerformanceFromRecordings _ [] = Nothing
buildPerformanceFromRecordings band (firstSrc : srcs) =
  let firstState =
        Builder
          band
          (get #srcDate firstSrc)
          (get #srcVenue firstSrc)
          (get #srcCity firstSrc)
          (get #srcState firstSrc)
          srcs
   in pure $ State.evalState (helper firstSrc) firstState
  where
    helper :: RecordingData -> State Builder Performance
    helper firstSrc = do
      (Builder band date venue city state srcs) <- State.get
      case srcs of
        [] ->
          pure $
            newRecord @Performance
              |> set #date date
              |> set #venue venue
              |> set #city city
              |> set #state state
              |> set #bandId (get #id band)
        (src : srcs) ->
          let nextVenue = if venue == "Unknown" then srcVenue src else venue
              nextCity = if city == "Unknown" then srcCity src else city
              nextState = if state == "Unknown" then srcState src else state
           in do
                State.put (Builder band date nextVenue nextCity nextState srcs)
                helper firstSrc

makeRecordingRecord performanceId RecordingData {..} =
  newRecord @Recording
    |> set #identifier srcIdentifier
    |> set #performanceId performanceId
    |> set #transferer srcTransferer
    |> set #source srcSource
    |> set #lineage srcLineage
    |> set #archiveDownloads srcDownloads
    |> set #avgRating srcAvgRating
    |> set #numReviews srcNumReviews

makeRecordingRecord' :: (?modelContext :: ModelContext) => Band -> RecordingData -> IO Recording
makeRecordingRecord' band recording@RecordingData {..} = do
  performanceId <- sqlQuery performanceIdQuery [srcIdentifier, srcDate]

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