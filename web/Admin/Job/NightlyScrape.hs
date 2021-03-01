module Admin.Job.NightlyScrape where

import Admin.Controller.Prelude
import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Helper.Queries
import Application.Script.Prelude
import Control.Exception
import Control.Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.Set as Set
import qualified Data.List as List
import IHP.Log.Types
import qualified IHP.Log as Log
import System.IO (hFlush, stdout)

instance Job NightlyScrapeJob where
    perform NightlyScrapeJob { .. } = do
        band <- fetch bandId
        putStrLn $ "Performing nightly scrape job for " <> get #collection band <> "..."

        runBand band
        hFlush stdout
        pure ()

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
    -- get recently uploaded recordings, as RecordingData
    recordingDatas <- (map (archiveToAttics (get #collection band)) . map fst) <$> archiveSearch (get #collection band) PublicDate
    currentIdentifiers <- Set.fromList <$> identifiersForBand band
    filterNew recordingDatas currentIdentifiers
        |> mapM (getOrBuildRecordingFromData band)
        >>= createMany
    where
        filterNew datas ids = filter (\d -> not $ Set.member (get #identifier d) ids) datas

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
    let ?context = ?modelContext
    recordingDatas <- map (archiveToAttics (get #collection band)) . map fst <$> searchArchive (get #collection band) PublicDate
    case recordingDatas of
        [] -> do
            Log.info $ "No recent reviews found for " <> get #collection band <> ". Skipping."
            pure []
        recentlyReviewed -> do
            recordings <- mapM getRecording recentlyReviewed
            let zipped = catMaybes $ map (\(a, b) -> a >>= \a' -> pure (a', b)) (zip recordings recentlyReviewed)
            updated <- mapM updateRecording zipped
            Log.info $ "Updated " <> show (List.length updated) <> " recordings for " <> get #collection band
            pure updated

addUpdate :: Band -> Script
addUpdate band = do
  now <- getCurrentTime
  void $
    band
      |> set #updatedAt now
      |> updateRecord

getRecording :: (?modelContext :: ModelContext) => RecordingData -> IO (Maybe Recording)
getRecording recording = do
    query @Recording
      |> filterWhere (#identifier, get #identifier recording)
      |> fetchOneOrNothing

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

    where
        filterSinceTime :: [(ArchiveItem, UTCTime)] -> UTCTime -> [ArchiveItem]
        filterSinceTime recordingsWithTimes updatedAt =
            let newIds = filter (\(_, time) -> time > updatedAt) recordingsWithTimes
            in map fst newIds

        mapFst :: (a -> b) -> (a, c) -> (b, c)
        mapFst f (a, c) = (f a, c)
