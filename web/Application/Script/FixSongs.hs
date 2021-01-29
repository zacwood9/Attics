#!/usr/bin/env run-script
module Application.Script.FixSongs where

import Application.Script.Prelude
import qualified IHP.Log as Log
import qualified Data.List as List
import Database.PostgreSQL.Simple (Only(..), Query)
import Database.PostgreSQL.Simple.SqlQQ
import Application.Helper.Archive

run :: Script
run = do
    recordings :: [Recording] <- sqlQuery firstQuery ()
    _ :: [[Song]] <- mapM getSongRecords recordings
    mapIOLog_
        (\i total recording -> show i <> "/" <> show total <> ": " <> get #identifier recording)
        getSongRecords
        recordings

-- | get all recordings that have songs with file names as their titles
firstQuery :: Query
firstQuery = [sql|
    SELECT DISTINCT ON (recordings.identifier) recordings.*
    FROM songs
    INNER JOIN recordings ON recordings.id = songs.recording_id
    WHERE songs.title = songs.file_name
|]

getSongRecords :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => Recording -> IO [Song]
getSongRecords recording = do
  currentSongs <- query @Song
    |> filterWhere (#recordingId, get #id recording)
    |> orderBy #track
    |> fetch

  (ItemFiles mp3s originals _) <- getItemFiles (get #identifier recording)
  if List.length currentSongs == List.length originals
    then do
        songs <- zip3 mp3s originals currentSongs
            |> mapM (makeSongRecord' recording)
        pure songs
    else pure currentSongs

makeSongRecord :: (?modelContext :: ModelContext) => Recording -> ArchiveSong -> Song
makeSongRecord recording ArchiveSong {..} =
  newRecord @Song
    |> set #title atticsSongTitle
    |> set #album atticsSongAlbum
    |> set #creator atticsSongCreator
    |> set #length atticsSongLength
    |> set #track atticsSongTrack
    |> set #fileName atticsSongFileName
    |> set #recordingId (get #id recording)

makeSongRecord' :: (?modelContext :: ModelContext) => Recording -> (ArchiveSong, ArchiveSong, Song) -> IO Song
makeSongRecord' recording (mp3, original, song) =
  song
    |> set #title (get #atticsSongTitle original)
    |> set #album (get #atticsSongAlbum original)
    |> set #creator (get #atticsSongCreator original)
    |> set #length (get #atticsSongLength original)
    |> set #track (get #atticsSongTrack original)
    |> set #fileName (get #atticsSongFileName mp3)
    |> updateRecord