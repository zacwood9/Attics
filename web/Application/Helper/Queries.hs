module Application.Helper.Queries where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Generated.Types
import IHP.ControllerPrelude
import Network.HTTP.Types.Status
import Network.Wai
import Application.Types
import Control.Exception (try)

fetchBandByCollection :: (?modelContext :: ModelContext) => Collection -> IO Band
fetchBandByCollection collection =
  query @Band |> filterWhere (#collection, collection) |> fetchOne

identifiersForBand :: _ => Band -> IO [Identifier]
identifiersForBand band = do
    map fromOnly <$> sqlQuery theQuery (Only (get #id band))
    where
        theQuery = [sql|
            SELECT recordings.identifier
            FROM recordings
            INNER JOIN performances ON recordings.performance_id = performances.id
            INNER JOIN bands ON performances.band_id = bands.id
            WHERE bands.id = ?
        |]


