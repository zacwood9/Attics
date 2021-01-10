module Application.Helper.Controller
  ( -- To use the built in login:
    -- module IHP.LoginSupport.Helper.Controller
    badRequest,
    fetchBand,
    fetchBands,
    fetchTopPerformances,
    fetchPerformance,
    fetchPerformance',
    fetchPerformances,
    fetchRecordings,
  )
where

-- Here you can add functions which are available in all your controllers

-- To use the built in login:
-- import IHP.LoginSupport.Helper.Controller

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Generated.Types
import IHP.ControllerPrelude
import Network.HTTP.Types.Status
import Network.Wai
import Web.Types

badRequest :: IO ()
badRequest = respondAndExit $ responseLBS status403 [] "bad request"

fetchBand :: (?modelContext :: ModelContext) => Collection -> IO Band
fetchBand collection =
  query @Band |> filterWhere (#collection, collection) |> fetchOne

fetchTopPerformances :: (?modelContext :: ModelContext) => Collection -> Int -> IO (HashMap Year [PerformanceWithMetadata])
fetchTopPerformances collection n = do
  performances <- fetchAllPerformances collection
  return $ takeN (group performances) n
  where
    group =
      foldr
        ( \p@PerformanceWithMetadata {performance = Performance {date}} ->
            HM.insertWith (++) (Text.take 4 date) [p]
        )
        HM.empty

    takeN hm n =
      hm
        |> HM.toList
        |> map (\(y, shows) -> (y, shows |> sortByStars |> take n))
        |> HM.fromList

    sortByStars = sortBy (\a b -> stars b `compare` stars a)
    stars PerformanceWithMetadata {..} = avgRating * fromIntegral numReviews

fetchPerformance :: (?modelContext :: ModelContext) => Collection -> Date -> IO PerformanceWithMetadata
fetchPerformance collection date = do
  result <- sqlQuery performancesQuery [collection, date ++ "%"]
  case result of
    [] -> error "performance not found"
    (performance : _) -> pure performance

fetchPerformance' :: (?modelContext :: ModelContext) => Id Performance -> IO PerformanceWithMetadata
fetchPerformance' id = do
  result <- sqlQuery performanceQuery [id]
  case result of
    [] -> error "performance not found"
    (performance : _) -> pure performance

fetchPerformances :: (?modelContext :: ModelContext) => Collection -> Year -> IO [PerformanceWithMetadata]
fetchPerformances collection date = do
  sqlQuery performancesQuery [collection, date ++ "%"]

fetchAllPerformances :: (?modelContext :: ModelContext) => Collection -> IO [PerformanceWithMetadata]
fetchAllPerformances collection = do
  sqlQuery allPerformancesQuery [collection]

instance FromRow PerformanceWithMetadata where
  fromRow =
    PerformanceWithMetadata
      <$> fromRow -- Performance as stored in DB
      <*> field -- following are computed fields in query
      <*> field
      <*> field
      <*> field
      <*> field

performancesQuery =
  [sql|
    SELECT performances.*,
    SUM(recordings.num_reviews * recordings.avg_rating) / (CASE SUM(recordings.num_reviews) WHEN 0 THEN 1 ELSE sum(recordings.num_reviews) END) as avg_rating,
    SUM(recordings.num_reviews) as num_reviews,
    COUNT(recordings.id) as num_recordings,
    SUM(recordings.archive_downloads) as archive_downloads,
    SUM(recordings.attics_downloads) as attics_downloads
    FROM performances
    INNER JOIN bands ON performances.band_id = bands.id
    RIGHT JOIN recordings ON recordings.performance_id = performances.id
    WHERE bands.collection = ? AND performances.date LIKE ?
    GROUP BY performances.id
    ORDER BY performances.date
|]

performanceQuery =
  [sql|
    SELECT performances.*,
    SUM(recordings.num_reviews * recordings.avg_rating) / (CASE SUM(recordings.num_reviews) WHEN 0 THEN 1 ELSE sum(recordings.num_reviews) END) as avg_rating,
    SUM(recordings.num_reviews) as num_reviews,
    COUNT(recordings.id) as num_recordings,
    SUM(recordings.archive_downloads) as archive_downloads,
    SUM(recordings.attics_downloads) as attics_downloads
    FROM performances
    INNER JOIN bands ON performances.band_id = bands.id
    RIGHT JOIN recordings ON recordings.performance_id = performances.id
    WHERE performances.id = ?
    GROUP BY performances.id
    ORDER BY performances.date
|]

allPerformancesQuery =
  [sql|
    SELECT performances.*,
    SUM(recordings.num_reviews * recordings.avg_rating) / (CASE SUM(recordings.num_reviews) WHEN 0 THEN 1 ELSE sum(recordings.num_reviews) END) as avg_rating,
    SUM(recordings.num_reviews) as num_reviews,
    COUNT(recordings.id) as num_recordings,
    SUM(recordings.archive_downloads) as archive_downloads,
    SUM(recordings.attics_downloads) as attics_downloads
    FROM performances
    INNER JOIN bands ON performances.band_id = bands.id
    RIGHT JOIN recordings ON recordings.performance_id = performances.id
    WHERE bands.collection = ?
    GROUP BY performances.id
    ORDER BY performances.date
|]

fetchRecordings :: (?modelContext :: ModelContext) => Collection -> Date -> IO [Recording]
fetchRecordings collection date = do
  sqlQuery recordingsQuery [collection, date]

recordingsQuery =
  [sql|
        SELECT recordings.* FROM recordings
        INNER JOIN performances ON recordings.performance_id = performances.id
        INNER JOIN bands ON performances.band_id = bands.id
        WHERE bands.collection = ? AND performances.date = ?
    |]


instance FromRow BandWithMetadata  where
  fromRow =
    BandWithMetadata
      <$> fromRow -- Band as stored in DB
      <*> field -- following are computed fields in query
      <*> field

fetchBands :: (?modelContext :: ModelContext ) => IO [BandWithMetadata]
fetchBands = sqlQuery bandsQuery ()
  where bandsQuery = [sql|
    SELECT
      bands.id, collection, name, updated_at, url, logo_url,
      count(DISTINCT performances.id) as num_performances,
      count(recordings.id) as num_recordings
    FROM bands
    LEFT JOIN performances ON bands.id = performances.band_id
    LEFT JOIN recordings ON performances.id = recordings.performance_id
    GROUP BY bands.id
  |]