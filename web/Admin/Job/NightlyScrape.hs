module Admin.Job.NightlyScrape where

import Admin.Controller.Prelude
import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Script.Prelude
import Control.Exception
import Control.Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.List as List
import System.Log.FastLogger

instance Job NightlyScrapeJob where
    perform NightlyScrapeJob { .. } = do
        band <- fetch bandId
        putStrLn $ "Performing nightly scrape job for " <> get #collection band <> "..."
        runBand band

    maxAttempts = 2

runBand :: Band -> Script
runBand band = do
    recordings <- addNewRecordings band advancedSearch
    addSongsForRecordings recordings getArchiveFiles
    updateRecentlyReviewedRecordings band advancedSearch
    addUpdate band

-- | Gets latest recordings added, then adds them to the database.
-- Creates performance for recording if necessary.
addNewRecordings
    :: (?modelContext :: ModelContext)
    => Band
    -> (Text -> AdvancedSearchSort -> IO [(ArchiveItem, UTCTime)])
    -> IO [Recording]
addNewRecordings band archiveSearch = do
    -- get recently uploaded recordings
    result <- getNewRecordingsFromArchive band PublicDate archiveSearch

    case result of
        [] -> do
            putStrLn $ "No recent recordings found for " <> get #collection band <> ". Skipping."
            pure []
        recentRecordingData -> do
            recordingDatas <- mapM (getOrCreateRecordingFromData band) recentRecordingData
            created <- mapM
                (\record -> do
                    result <- try (create record)
                    case result of
                        Left (e :: SomeException) -> do
                            putStrLn $ "failed to insert recent recording "
                                <> get #identifier record
                                <> ": " <> show e
                            pure Nothing
                        Right r -> pure $ Just r)
                recordingDatas
            pure (catMaybes created)


-- | Given a list of recordings, fetch their songs and add them to the database.
addSongsForRecordings
    :: (?modelContext :: ModelContext)
    => [Recording]
    -> (Text -> IO [ArchiveFile])
    -> IO [Song]
addSongsForRecordings recordings getFiles = do
    recordings
        |>  mapM (\r -> addSongsForRecording r getFiles)
        >>= pure . concat

addSongsForRecording
    :: (?modelContext :: ModelContext)
    => Recording
    -> (Text -> IO [ArchiveFile])
    -> IO [Song]
addSongsForRecording recording getFiles = do
    songs <- getSongsForRecording recording getFiles
    mapM createRecord songs

updateRecentlyReviewedRecordings
    :: (?modelContext :: ModelContext)
    => Band
    -> (Text -> AdvancedSearchSort -> IO [(ArchiveItem, UTCTime)])
    -> IO [Recording]
updateRecentlyReviewedRecordings band searchArchive = do
  result <- getNewRecordingsFromArchive band ReviewDate searchArchive
  case result of
    [] -> do
        putStrLn $ "No recent reviews found for " <> get #collection band <> ". Skipping."
        pure []
    recentlyReviewed -> do
      recordings <- mapM getRecording recentlyReviewed
      updated <- mapM updateRecording (zip recordings recentlyReviewed)
      putStrLn $ "Updated " <> show (List.length updated) <> " recordings for " <> get #collection band
      pure updated

addUpdate :: Band -> Script
addUpdate band = do
  now <- getCurrentTime
  void $
    band
      |> set #updatedAt now
      |> updateRecord

getRecording :: (?modelContext :: ModelContext) => RecordingData -> IO Recording
getRecording recording = do
  recording <-
    query @Recording
      |> filterWhere (#identifier, get #identifier recording)
      |> fetchOneOrNothing

  case recording of
    Nothing -> error "could not find recording for given data"
    Just recording -> pure recording

updateRecording :: (?modelContext :: ModelContext) => (Recording, RecordingData) -> IO Recording
updateRecording (recording, newData) = do
  recording
    |> set #avgRating (get #avgRating newData)
    |> set #numReviews (get #numReviews newData)
    |> set #archiveDownloads (get #downloads newData)
    |> updateRecord

-- | Given a band and a way to sort, use AdvancedSearch
-- to get the latest recordings and return them as RecordingData.
getNewRecordingsFromArchive
    :: Band
    -> AdvancedSearchSort
    -> (Text -> AdvancedSearchSort -> IO [(ArchiveItem, UTCTime)])
    -> IO [RecordingData]
getNewRecordingsFromArchive band sort searchArchive = do
    result <- searchArchive (get #collection band) sort
    filterSinceTime result (get #updatedAt band)
        |> map (archiveToAttics (get #collection band))
        |> pure

filterSinceTime :: [(ArchiveItem, UTCTime)] -> UTCTime -> [ArchiveItem]
filterSinceTime recordingsWithTimes updatedAt =
  let newIds = filter (\(_, time) -> time > updatedAt) recordingsWithTimes
   in map fst newIds

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

