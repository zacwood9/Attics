module Test.TestFixSongs where

import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.FrameworkConfig (ConfigBuilder(..))
import           IHP.Prelude
import           IHP.QueryBuilder

import           Web.Types
import           Web.Routes
import           Generated.Types
import           Main ()
import qualified Config
import qualified Data.List as List
import Application.Helper.Archive
import Application.Helper.Scrape
import qualified Admin.Job.FixSong as Job
import IHP.ModelSupport
import IHP.FetchRelated
import Control.Exception (bracket,evaluate, SomeException(..))

spec :: Spec
spec = describe "TestFixSongs" $ do
    describe "#addNewRecordings" $ do
        beforeAll (Config.test |> mockContext WebApplication) $ do
            it "creates new performance and recording" $ withContext do
                bracket
                    (do
                        band <- testBand |> createRecord
                        p <- testPerformance band |> createRecord
                        r <- testRecording p |> createRecord
                        mapM_ createRecord (testSongs r)
                        pure (band,r))
                    (cleanupBand)
                    (\(band, r) -> do
                        Job.fixSongsForBand mockGetFiles band
                        songs <- query @Song
                            |> filterWhere (#recordingId, get #id r)
                            |> orderBy #track
                            |> fetch
                        List.length songs `shouldBe` 2
                        let (a:b:_) = songs
                        get #title a `shouldBe` "File 1"
                        get #title b `shouldBe` "File 2"
                        )

cleanupBand :: (?modelContext :: ModelContext) => (Band, Recording) -> IO ()
cleanupBand (band, _) = do
    perfs <- query @Performance |> filterWhere (#bandId, get #id band) |> fetch
    recs <- mapM (\p -> query @Recording |> filterWhere (#performanceId, get #id p) |> fetch) perfs
    perfs |> deleteRecords
    mapM_ (\rs -> deleteRecords rs) recs
    band |> deleteRecord

testBand = newRecord @Band
    |> set #collection "GratefulDead"
    |> set #name "Grateful Dead"

testPerformance band = newRecord @Performance
    |> set #bandId (get #id band)
    |> set #date "2020-01-03"
    |> set #venue "Eagle Bank Arena"
    |> set #city "Fairfax"
    |> set #state "VA"

testRecording performance = newRecord @Recording
    |> set #performanceId (get #id performance)
    |> set #identifier "item3"
    |> set #transferer "transferer"
    |> set #source ""
    |> set #lineage ""
    |> set #avgRating 0
    |> set #numReviews 0
    |> set #archiveDownloads 0

testSongs :: Recording -> [Song]
testSongs recording = [
    newRecord @Song
        |> set #fileName "file1.mp3"
        |> set #title "file1.mp3"
        |> set #track 1
        |> set #creator ""
        |> set #length ""
        |> set #album ""
        |> set #recordingId (get #id recording),
    newRecord @Song
        |> set #fileName "file2.mp3"
        |> set #title "File 2"
        |> set #track 1
        |> set #creator ""
        |> set #length ""
        |> set #album ""
        |> set #recordingId (get #id recording)
    ]

item3Files = [
    ArchiveFile {
        afFileName = "file1.mp3",
        afCreator = Nothing,
        afTitle = Just "File 1",
        afTrack = Just "1",
        afAlbum = Nothing,
        afLength = Just "",
        afOriginal = "file1.flac",
        afFormat = Nothing
    },
    ArchiveFile {
        afFileName = "file1.flac",
        afCreator = Nothing,
        afTitle = Just "File 1",
        afTrack = Just "1",
        afAlbum = Nothing,
        afLength = Just "",
        afOriginal = Nothing,
        afFormat = Nothing
    },
    ArchiveFile {
        afFileName = "file2.mp3",
        afCreator = Nothing,
        afTitle = Nothing,
        afTrack = Nothing,
        afAlbum = Nothing,
        afLength = Just "123123",
        afOriginal = "file2.flac",
        afFormat = Nothing
    },
    ArchiveFile {
        afFileName = "file2.flac",
        afCreator = Nothing,
        afTitle = Just "File 2",
        afTrack = Just "2",
        afAlbum = Nothing,
        afLength = Just "1:23",
        afOriginal = Nothing,
        afFormat = Nothing
    }
    ]

mockGetFiles :: Text -> IO [ArchiveFile]
mockGetFiles _ = pure item3Files

