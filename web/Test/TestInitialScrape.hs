module Test.TestInitialScrape where

import           Test.Hspec
import           IHP.Test.Mocking
import           IHP.FrameworkConfig (ConfigBuilder(..))
import           IHP.Prelude
import           IHP.QueryBuilder
import IHP.Fetch

import           Web.Types
import           Web.Routes
import           Generated.Types
import           Main ()
import qualified Config
import qualified Data.List as List
import Application.Helper.Archive
import Application.Helper.Scrape
import Application.Helper.Controller
import qualified Admin.Job.InitialScrape as Job
import IHP.ModelSupport
import IHP.FetchRelated
import Control.Exception (try, bracket,evaluate, SomeException(..))

spec :: Spec
spec = describe "Initial Scrape Job" $ do
    aroundAll (withIHPApp WebApplication Config.test) $ do
        it "adds new performances and recordings and songs" $ withContext do
            bracket
                (do
                    band <- testBand |> createRecord
                    testPerformance band |> create
                    pure band)
                cleanupBand
                (\band -> do
                    Job.initialScrape mockScrape mockGetFiles band

                    -- test performance was added for each date
                    ps <- query @Performance
                        |> filterWhere (#bandId, get #id band)
                        |> fetch
                    List.length ps `shouldBe` 3

                    let (p:_) = filter (\p -> get #date p == "2020-01-01") ps
                    pWithMeta <- fetchPerformanceWithMetadataFromId (get #id p)
                    get #avgRating pWithMeta `shouldBe` (4.5 :: Double)

                    let (p1:_) = filter (\p -> get #date p == "2020-01-02") ps
                    p1WithMeta <- fetchPerformanceWithMetadataFromId (get #id p1)
                    get #avgRating p1WithMeta `shouldBe` (0 :: Double)

                    -- test recording added for each item
                    rs <- mapM
                        (\p -> query @Recording
                                |> filterWhere (#performanceId, get #id p)
                                |> fetch)
                        ps >>= pure . concat
                    List.length rs `shouldBe` 4

                    -- test songs were added to recording 3
                    mapM_
                        (\recording -> do
                            songs <- query @Song
                                |> filterWhere (#recordingId, get #id recording)
                                |> orderBy #track
                                |> fetch
                            case get #identifier recording of
                                "3" -> do
                                    List.length songs `shouldBe` 3
                                    let (a:b:_) = songs
                                    get #title a `shouldBe` "File 1"
                                    get #title b `shouldBe` "File 2"
                                _ -> List.length songs `shouldBe` 0
                            pure ()
                        ) rs
                )

cleanupBand :: (?modelContext :: ModelContext) => Band -> IO ()
cleanupBand band = do
    perfs <- query @Performance |> filterWhere (#bandId, get #id band) |> fetch
    recs <- mapM (\p -> query @Recording |> filterWhere (#performanceId, get #id p) |> fetch) perfs
    perfs |> deleteRecords
    mapM_ deleteRecords recs
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


items :: [ArchiveItem] = [
    def { identifier = "1", date = "2020-01-01", collection = "GratefulDead", numReviews = Just 1, avgRating = Just "5.0" },
    def { identifier = "2", date = "2020-01-01", collection = "GratefulDead", numReviews = Just 1, avgRating = Just "4.0" },
    def { identifier = "3", date = "2020-01-01", collection = "GratefulDead" },
    def { identifier = "4", date = "2020-01-02", collection = "GratefulDead" },
    def { identifier = "5", date = "2020-01-03", collection = "GratefulDead" }
    ]

mockScrape :: Text -> IO [ArchiveItem]
mockScrape _ = pure items

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
    },
    ArchiveFile {
        afFileName = "file3.mp3",
        afCreator = Nothing,
        afTitle = Nothing,
        afTrack = Nothing,
        afAlbum = Nothing,
        afLength = Just "123123",
        afOriginal = Nothing,
        afFormat = Nothing
    }
    ]

mockGetFiles :: Text -> IO [ArchiveFile]
mockGetFiles "3" = pure item3Files
mockGetFiles _ = pure []
