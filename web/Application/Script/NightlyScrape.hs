#!/usr/bin/env run-script

{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Application.Script.NightlyScrape where

import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Script.Prelude
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State as S
import qualified Data.List as L

run :: Script
run = do
  putStrLn "starting nightly scrape"
  bands <- query @Band |> fetch
  mapM_ runBand bands

runBand :: Band -> Script
runBand band = do
  addNewRecordings band
  updateRecentlyReviewedRecordings band
  addUpdate band

addNewRecordings :: Band -> Script
addNewRecordings band@Band {collection = c} = do
  result <- scrapeRecentRecordings band PublicDate
  case result of
    [] -> putStrLn $ "No recent recordings found for " <> c <> ". Skipping."
    recentRecordings -> do
      records <- mapM (makeRecordingRecord' band) recentRecordings >>= createMany
      putStrLn $ "Created " <> show (L.length records) <> " new recordings for " <> c

updateRecentlyReviewedRecordings :: Band -> Script
updateRecentlyReviewedRecordings band@Band {collection = c} = do
  result <- scrapeRecentRecordings band ReviewDate
  case result of
    [] -> putStrLn $ "No recent reviews found for " <> c <> ". Skipping."
    recentlyReviewed -> do
      recordings <- mapM getRecording recentlyReviewed
      updated <- mapM updateRecording (zip recordings recentlyReviewed)
      putStrLn $ "Updated " <> show (L.length updated) <> " recordings for " <> c

addUpdate :: Band -> Script
addUpdate band = do
  now <- getCurrentTime
  void $
    band
      |> set #updatedAt now
      |> updateRecord

getRecording :: (?modelContext :: ModelContext) => RecordingData -> IO Recording
getRecording RecordingData {..} = do
  recordings <-
    query @Recording
      |> filterWhere (#identifier, srcIdentifier)
      |> fetch

  case recordings of
    [] -> error "could not find recording for given data"
    (recording : _) -> pure recording

updateRecording :: (?modelContext :: ModelContext) => (Recording, RecordingData) -> IO Recording
updateRecording (recording, RecordingData {..}) = do
  recording
    |> set #avgRating srcAvgRating
    |> set #numReviews srcNumReviews
    |> set #archiveDownloads srcDownloads
    |> updateRecord

scrapeRecentRecordings :: Band -> AdvancedSearchSort -> IO [RecordingData]
scrapeRecentRecordings band@Band {..} sort = do
  recentRecordings <- advancedSearch collection sort
  let s = sourcesSinceLastUpdate recentRecordings band
  pure $ map (archiveToAttics collection) s

sourcesSinceLastUpdate :: [(ArchiveItem, UTCTime)] -> Band -> [ArchiveItem]
sourcesSinceLastUpdate recordingsWithTimes Band {updatedAt} =
  let newIds = filter (\(_, time) -> time > updatedAt) recordingsWithTimes
   in map fst newIds

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

scrapeSongsForBand :: (?modelContext :: ModelContext) => Band -> IO ()
scrapeSongsForBand Band {id = bandId} = do
  srcs <- sqlQuery "select recordings.* from recordings inner join performances on recordings.performance_id = performances.id inner join bands on performances.band_id = bands.id where bands.id = ?" (Only bandId)
  putStrLn $ "got " <> cs (show (L.length srcs)) <> " sources"
  recordingsAndSongs <- evalStateT scrapeSongsForRecordings (1, L.length srcs, srcs, False)
  putStrLn $ show $ L.length recordingsAndSongs
  putStrLn "done scraping"
  return ()

type ScrapeState = (Int, Int, [Recording], Bool)

scrapeSongsForRecordings :: StateT ScrapeState IO [(Recording, [ArchiveSong])]
scrapeSongsForRecordings = do
  (num, total, srcs, _) <- S.get
  case srcs of
    [] -> pure []
    (src : rest) -> do
      songs <- scrapeSongs
      put (num + 1, total, rest, False) -- move to the next source
      rest <- scrapeSongsForRecordings -- get the list of all the rest
      pure $ [(src, songs)] ++ rest -- combine current + rest

scrapeSongs :: StateT ScrapeState IO [ArchiveSong]
scrapeSongs = do
  (num, total, srcs@(Recording {..} : _), alreadyTried) <- S.get

  result <- liftIO $ do
    putStrLn $ "scraping " <> cs (show num) <> "/" <> cs (show total) <> " " <> identifier
    result <- try $ getItemFiles identifier
    case result of
      Right (ItemFiles mp3s _ _) -> pure $ Just mp3s
      Left (SomeException e) -> do
        putStrLn $ "failed to get source info: " <> identifier <> ", retrying..."
        putStrLn $ show e
        pure Nothing

  case result of
    Just mp3s -> pure mp3s
    Nothing ->
      if alreadyTried
        then do
          liftIO $ putStrLn $ "scrape " <> identifier <> " FAILED."
          pure []
        else do
          put (num, total, srcs, True)
          scrapeSongs
