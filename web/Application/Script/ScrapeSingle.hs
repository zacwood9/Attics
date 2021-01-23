#!/usr/bin/env run-script

module Application.Script.ScrapeSingle where

import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Script.Prelude
import Control.Exception (SomeException (..), try)
import Control.Monad (void)
import qualified Control.Monad.Trans.State as State (get)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T (pack, splitOn, strip, takeWhile, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Text.Read (readMaybe)

run :: Script
run = do
  args <- getArgs
  case args of
    (collection : _) -> initialScrape' collection
    [] -> error "ScrapeSingle requires one argument"
  where
    initialScrape' band =
      initialScrape band `catch` \(e :: SomeException) ->
        putStrLn $ "Unable to scrape " <> band <> ": " <> show e

    initialScrape collection = do
      band <- query @Band
        |> filterWhere (#collection, collection)
        |> fetchOne
      withPerfs <- fetchRelated #performances band

      deleteRecords (get #performances withPerfs)

      (performances, recordings) <- scrapeCollection band
      dbPerformances <- performances |> createMany
      dbRecordings <-
        map (makeRecordingRecord'' dbPerformances) recordings
          |> catMaybes
          |> createMany

      mapIOLog_
        (\i total recording -> show i <> "/" <> show total <> ": " <> get #identifier recording)
        ( \recording -> do
            songs <- try $ getSongRecords recording
            case songs of
              Left (SomeException _) -> putStrLn ("Failed to get songs for " <> get #identifier recording)
              Right songs -> unless (null songs) $ void $ createMany songs
        )
        dbRecordings

      putStrLn $
        "Added " <> show (L.length dbPerformances) <> " performances and "
          <> show (L.length dbRecordings)
          <> " recordings for "
          <> collection
          <> "."

      return ()

    makeRecordingRecord'' perfs recording = do
      performance <- findPerformance (get #date recording) perfs
      let record = makeRecordingRecord (get #id performance) recording
      pure record

    findPerformance date perfs = filter (\Performance {date = date'} -> date == date') perfs |> head

-- | scrape gets all sources in a collection and creates shows out of sources on the same date.
scrapeCollection :: Band -> IO ([Performance], [RecordingData])
scrapeCollection band = do
  items <- scrape (get #collection band)
  let sources = map (archiveToAttics (get #collection band)) items
  let shows = buildShowsFromSources band sources
  pure (shows, sources)

buildShowsFromSources :: Band -> [RecordingData] -> [Performance]
buildShowsFromSources band srcs =
  mapMaybe (buildPerformanceFromRecordings band) groupedSources
  where
    groupedSources = HM.elems $ buildDateMap srcs

buildDateMap :: [RecordingData] -> HashMap Text [RecordingData]
buildDateMap =
  foldr
    (\src -> HM.insertWith (++) (get #date src) [src])
    HM.empty

getSongRecords :: Recording -> IO [Song]
getSongRecords recording = do
  (ItemFiles mp3s _ _) <- getItemFiles (get #identifier recording)
  pure $ map (makeSongRecord recording) mp3s

mapIOLog_ :: (Int -> Int -> a -> Text) -> (a -> IO b) -> [a] -> IO ()
mapIOLog_ logF f as =
  let total = L.length as
   in mapIOLog' 1 total logF f as
  where
    mapIOLog' _ _ _ _ [] = pure ()
    mapIOLog' i total logF f (a : as) = do
      putStrLn $ logF i total a
      f a
      mapIOLog' (i + 1) total logF f as
