module Admin.Job.InitialScrape where
import Admin.Controller.Prelude
import Application.Helper.Archive
import Application.Helper.Scrape
import System.IO (hFlush, stdout, hSetEncoding)
import Application.Script.Prelude
import GHC.IO.Encoding
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.List        as List

instance Job InitialScrapeJob where
    perform InitialScrapeJob  { .. } = do
        band <- fetch bandId
        putStrLn $ "Initial scrape for " <> get #name band <> "..."
        initialScrape scrape getArchiveFiles band

    maxAttempts = 2

initialScrape
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveItem])
    -> (Text -> IO [ArchiveFile])
    -> Band
    -> IO ()
initialScrape scrapeArchive getFiles band = do
    items <- scrapeArchive (get #collection band)
    buildDateList items
        |> map (\(date, items) -> buildPerformance band date items)
        |> catMaybes
        |> \pairs -> do
            putStrLn $ "Attemping to insert " <> show (List.length pairs) <> " performances."
            mapM_ (insertPerformance getFiles) pairs


buildPerformance :: Band -> Text -> [ArchiveItem] -> Maybe (Performance, [Recording])
buildPerformance band date items = do
    let recordingDatas = map (archiveToAttics (get #collection band)) items
    performance <- buildPerformanceFromRecordings band recordingDatas
    pure (performance, map makeRecordingRecord recordingDatas)

-- | Given a performance record, make sure one for its date and band
-- doesn't exist already. It might have been scraped earlier.
-- Then insert it and its recordings.
insertPerformance
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveFile])
    -> (Performance, [Recording])
    -> IO ()
insertPerformance getFiles (performance, recordings) = do
    band <- fetch (get #bandId performance)
    existingPerformance <- getPerformanceOrNothing band (get #date performance)
    case existingPerformance of
        Just _ -> do
            putStrLn $ "Already created performance for " <> get #date performance <> "."
        Nothing -> do
            insertedPerformance <- create performance
            putStrLn $ "Created performance for " <> get #date performance <> "."
            createdRecordings <- mapM create (associateTo insertedPerformance recordings)
            addSongsForRecordings getFiles createdRecordings
            pure ()

    where
        associateTo performance = let performanceId = get #id performance in
            map (set #performanceId performanceId)

-- | Given a list of recordings, fetch their songs and add them to the database.
addSongsForRecordings
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveFile])
    -> [Recording]
    -> IO [Song]
addSongsForRecordings getFiles recordings = do
    recordings
        |>  mapM (addSongsForRecording getFiles)
        >>= pure . concat

addSongsForRecording
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveFile])
    -> Recording
    -> IO [Song]
addSongsForRecording getFiles recording = do
    songs <- getSongsForRecording recording getFiles
    mapM createRecord songs

getPerformanceOrNothing
    :: (?modelContext :: ModelContext)
    => Band
    -> Text
    -> IO (Maybe Performance)
getPerformanceOrNothing band date = do
    queryResult <- sqlQuery performanceIdQuery [get #collection band, date]
    case queryResult of
        [] -> pure Nothing
        (performance : _) -> pure (Just performance)
    where
        performanceIdQuery =
            [sql|
                SELECT performances.* from performances
                INNER JOIN bands ON performances.band_id = bands.id
                WHERE bands.collection = ? AND performances.date = ?
            |]


buildDateList :: [ArchiveItem] -> [(Text, [ArchiveItem])]
buildDateList items =
  foldr
    (\item -> HashMap.insertWith (++) (get #date item) [item])
    HashMap.empty
    items
    |> HashMap.toList
