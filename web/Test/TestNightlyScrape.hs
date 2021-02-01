module Test.TestNightlyScrape where

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
import qualified Admin.Job.NightlyScrape as Job
import IHP.ModelSupport
import IHP.FetchRelated
import Control.Exception (bracket,evaluate, SomeException(..))

spec :: Spec
spec = describe "Nightly Scrape" $ do
    describe "#getNewRecordingsFromArchive" $ do
        it "gets archive items since band's last update" $ do
            newRecordings <- Job.getNewRecordingsFromArchive (gdBand time1) PublicDate mockSearch
            List.length newRecordings `shouldBe` 1
            let (r:_) = newRecordings
            get #date r `shouldBe` "2020-01-03"
        it "returns empty list if no items found" $ do
            newRecordings <- Job.getNewRecordingsFromArchive (gdBand time4) PublicDate mockSearch
            newRecordings `shouldBe` []


cleanupBand :: (?modelContext :: ModelContext) => Band -> IO ()
cleanupBand band = do
    perfs <- query @Performance |> filterWhere (#bandId, get #id band) |> fetch
    recs <- mapM (\p -> query @Recording |> filterWhere (#performanceId, get #id p) |> fetch) perfs
    perfs |> deleteRecords
    mapM_ (\rs -> deleteRecords rs) recs
    band |> deleteRecord

integration :: Spec
integration = describe "Nightly Scrape (Integration tests)" $ do
    describe "#addNewRecordings" $ do
        beforeAll (Config.test |> mockContext WebApplication) $ do
            it "creates new performance and recording" $ withContext do
                bracket
                    (gdBand time2 |> createRecord)
                    (cleanupBand)
                    (\band -> do
                        rAll@(r:_) <- Job.addNewRecordings band mockSearch
                        pAll@(p:_) <- query @Performance |> filterWhere (#bandId, get #id band) |> fetch
                        List.length pAll `shouldBe` 1
                        List.length rAll `shouldBe` 1

                        get #identifier r `shouldBe` "item3"
                        get #performanceId r `shouldBe` get #id p
                        get #bandId p `shouldBe` get #id band
                        )
            it "creates recording and reuses performance" $ withContext do
                bracket
                    (do
                        band <- gdBand time2 |> createRecord
                        newRecord @Performance
                            |> set #bandId (get #id band)
                            |> set #date (get #date item3)
                            |> createRecord
                        pure band
                        )
                    (cleanupBand)
                    (\band -> do
                        rAll@(r:_) <- Job.addNewRecordings band mockSearch
                        pAll@(p:_) <- query @Performance |> filterWhere (#bandId, get #id band) |> fetch
                        List.length pAll `shouldBe` 1
                        List.length rAll `shouldBe` 1

                        get #identifier r `shouldBe` "item3"
                        get #performanceId r `shouldBe` get #id p
                        get #bandId p `shouldBe` get #id band
                        )

    describe "#addSongsForRecording" $ do
        beforeAll (Config.test |> mockContext WebApplication) $ do
            it "creates songs" $ withContext do
                bracket
                    (do
                        band <- gdBand time2 |> createRecord
                        testPerformance band |> createRecord
                        pure band
                        )
                    (cleanupBand)
                    (\band -> do
                        rAll@(r:_) <- Job.addNewRecordings band mockSearch
                        List.length rAll `shouldBe` 1
                        songs <- Job.addSongsForRecordings rAll mockGetFiles
                        List.length songs `shouldBe` 2
                        )
            it "gets info from originals" $ withContext do
                bracket
                    (do
                        band <- gdBand time2 |> createRecord
                        testPerformance band |> createRecord
                        pure band
                        )
                    (cleanupBand)
                    (\band -> do
                        rAll@(r:_) <- Job.addNewRecordings band mockSearch
                        List.length rAll `shouldBe` 1
                        songs <- Job.addSongsForRecordings rAll mockGetFiles
                        song <- query @Song |> filterWhere (#fileName, "file2.mp3") |> fetchOne
                        get #recordingId song `shouldBe` get #id r
                        get #title song `shouldBe` "File 2"
                        get #track song `shouldBe` 2
                        get #length song `shouldBe` "1:23"
                        )

    describe "#updateRecentlyReviewedRecordings" $ do
        beforeAll (Config.test |> mockContext WebApplication) $ do
            it "updates recording with new info" $ withContext do
                bracket
                    (do
                        band <- gdBand time2 |> createRecord
                        p <- testPerformance band |> createRecord
                        testRecording p |> createRecord
                        pure band
                        )
                    (cleanupBand)
                    (\band -> do
                        recordings <- Job.updateRecentlyReviewedRecordings band mockSearch
                        List.length recordings `shouldBe` 1
                        let (r:_) = recordings
                        get #archiveDownloads r `shouldBe` 12345
                        get #avgRating r `shouldBe` 5
                        get #numReviews r `shouldBe` 123
                        )
            it "fails for unknown recording" $ withContext do
                bracket
                    (do
                        band <- gdBand time2 |> createRecord
                        p <- testPerformance band |> createRecord
                        pure band
                        )
                    (cleanupBand)
                    (\band -> do
                        Job.updateRecentlyReviewedRecordings band mockSearch `shouldThrow` anyException
                        )



gdBand :: UTCTime -> Band
gdBand time = newRecord @Band
    |> set #name "Grateful Dead"
    |> set #collection "GratefulDead"
    |> set #updatedAt time

item3 = ArchiveItem {
    identifier = "item3",
    date = "2020-01-03",
    collection = Just "GratefulDead",
    transferer = Nothing,
    downloads = Just 12345,
    source = Nothing,
    avgRating = Just "5",
    numReviews = Just 123,
    lineage = Nothing,
    coverage = Just "Fairfax, VA",
    venue = Just "Eagle Bank Arena"
}

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


mockSearch :: Text -> AdvancedSearchSort -> IO [(ArchiveItem, UTCTime)]
mockSearch c _ =
    pure [
        (def { collection = pure c } :: ArchiveItem, time1),
        (item3, time3)
        ]

mockGetFiles :: Text -> IO [ArchiveFile]
mockGetFiles _ = pure item3Files

time1 = parseTime' "2020-01-01"
time2 = parseTime' "2020-01-02"
time3 = parseTime' "2020-01-03"
time4 = parseTime' "2020-01-04"

parseTime' :: String -> UTCTime
parseTime' s = UTCTime (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s) 0
