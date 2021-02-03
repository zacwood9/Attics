module Admin.Job.FixSong where
import Admin.Controller.Prelude
import Application.Helper.Archive
import System.IO (hFlush, stdout, hSetEncoding)
import Application.Script.Prelude
import GHC.IO.Encoding

instance Job FixSongJob where
    perform FixSongJob { .. } = do
        band <- fetch bandId
        putStrLn $ "Fixing songs for " <> get #name band <> "..."
        fixSongsForBand getArchiveFiles band

    maxAttempts = 2

fixSongsForBand
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveFile])
    -> Band
    -> IO ()
fixSongsForBand searchArchive band = do
    hSetEncoding stdout utf8
    badRecordings <- getRecordingsWithBadSongNames band
    forEach badRecordings (fixRecording searchArchive)

fixRecording
    :: (?modelContext :: ModelContext)
    => (Text -> IO [ArchiveFile])
    -> Recording
    -> IO ()
fixRecording getArchiveFiles recording  = do
    currentSongs <- getCurrentSongs recording
    archiveSongs <- getSongsForRecording recording getArchiveFiles
    zip currentSongs archiveSongs
        |> mapM_ updateSong
    hFlush stdout
    pure ()

getRecordingsWithBadSongNames
    :: (?modelContext :: ModelContext)
    => Band
    -> IO [Recording]
getRecordingsWithBadSongNames band =
    sqlQuery query (Only (get #id band))
    where
        query = [sql|
            SELECT DISTINCT ON (recordings.identifier) recordings.*
            FROM songs
            INNER JOIN recordings ON recordings.id = songs.recording_id
            INNER JOIN performances ON performances.id = recordings.performance_id
            INNER JOIN bands ON bands.id = performances.band_id
            WHERE songs.title = songs.file_name AND bands.id = ?
        |]

getCurrentSongs
    :: (?modelContext :: ModelContext)
    => Recording
    -> IO [Song]
getCurrentSongs recording = query @Song
    |> filterWhere (#recordingId, get #id recording)
    |> orderBy #track
    |> fetch

updateSong
    :: (?modelContext :: ModelContext)
    => (Song, Song)
    -> IO ()
updateSong (current, new) =
    let oldFileName = get #title current in
    current
        |> set #title (get #title new)
        |> set #album (get #album new)
        |> set #creator (get #creator new)
        |> set #length (get #length new)
        |> set #track (get #track new)
        |> set #fileName (get #fileName new)
        |> updateRecord
        >> (putStrLn $ oldFileName <> " --> " <> get #title new)


