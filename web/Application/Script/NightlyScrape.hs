#!/usr/bin/env run-script

{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Application.Script.NightlyScrape where

import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Script.Prelude
import Control.Exception
import Control.Monad
import qualified Control.Monad.Trans.State as State
import qualified Data.List as List

run :: Script
run = do
  Log.info "starting nightly scrape"
  bands <- query @Band |> fetch
  mapM_ runBand bands

runBand :: Band -> Script
runBand band = do
  result <- try $ do
    addNewRecordings band
    updateRecentlyReviewedRecordings band
    addUpdate band
  case result of
    Left (e :: SomeException) -> Log.error $ "failed to update "
      <> get #collection band
      <> ": " <> show e
    Right _ -> pure ()

addNewRecordings :: Band -> Script
addNewRecordings band@Band {collection = c} = do
  result <- scrapeRecentRecordings band PublicDate
  case result of
    [] -> Log.info $ "No recent recordings found for " <> c <> ". Skipping."
    recentRecordings -> do
      records <- mapM (makeRecordingRecord' band) recentRecordings
      mapM_
        (\record -> do
          result <- try (create record)
          case result of
            Left (e :: SomeException) ->
              putStrLn $ "failed to insert recent recording "
                <> get #identifier record
                <> ": " <> show e
            Right _ -> pure ())
        records
      putStrLn $ "Created " <> show (List.length records) <> " new recordings for " <> c

updateRecentlyReviewedRecordings :: Band -> Script
updateRecentlyReviewedRecordings band@Band {collection = c} = do
  result <- scrapeRecentRecordings band ReviewDate
  case result of
    [] -> putStrLn $ "No recent reviews found for " <> c <> ". Skipping."
    recentlyReviewed -> do
      recordings <- mapM getRecording recentlyReviewed
      updated <- mapM updateRecording (zip recordings recentlyReviewed)
      putStrLn $ "Updated " <> show (List.length updated) <> " recordings for " <> c

addUpdate :: Band -> Script
addUpdate band = do
  now <- getCurrentTime
  void $
    band
      |> set #updatedAt now
      |> updateRecord

getRecording :: (?modelContext :: ModelContext) => RecordingData -> IO Recording
getRecording recording = do
  recordings <-
    query @Recording
      |> filterWhere (#identifier, get #identifier recording)
      |> fetch

  case recordings of
    [] -> error "could not find recording for given data"
    (recording : _) -> pure recording

updateRecording :: (?modelContext :: ModelContext) => (Recording, RecordingData) -> IO Recording
updateRecording (recording, newData) = do
  recording
    |> set #avgRating (get #avgRating newData)
    |> set #numReviews (get #numReviews newData)
    |> set #archiveDownloads (get #downloads newData)
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
  putStrLn $ "got " <> cs (show (List.length srcs)) <> " sources"
  recordingsAndSongs <- State.evalStateT scrapeSongsForRecordings (1, List.length srcs, srcs, False)
  putStrLn $ show $ List.length recordingsAndSongs
  putStrLn "done scraping"
  return ()

type ScrapeState = (Int, Int, [Recording], Bool)

scrapeSongsForRecordings :: State.StateT ScrapeState IO [(Recording, [ArchiveSong])]
scrapeSongsForRecordings = do
  (num, total, srcs, _) <- State.get
  case srcs of
    [] -> pure []
    (src : rest) -> do
      songs <- scrapeSongs
      State.put (num + 1, total, rest, False) -- move to the next source
      rest <- scrapeSongsForRecordings -- get the list of all the rest
      pure $ (src, songs) : rest -- combine current + rest

scrapeSongs :: State.StateT ScrapeState IO [ArchiveSong]
scrapeSongs = do
  (num, total, srcs@(Recording {..} : _), alreadyTried) <- State.get

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
          State.put (num, total, srcs, True)
          scrapeSongs
