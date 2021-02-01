module Web.Controller.Attics where

import Web.Controller.Prelude
import Web.View.Attics.Bands
import Web.View.Attics.Performances
import Web.View.Attics.Recordings
import Data.Text
import Web.View.Prelude (HomeView (..))

renderHome :: forall controller. (?context::ControllerContext, ?theAction :: controller, ?modelContext :: ModelContext) => IO ()
renderHome = do
    bands <- query @Band |> fetchCount
    shows <- query @Performance |> fetchCount
    render HomeView { .. }

isJsonRequest :: (?context :: ControllerContext) => Bool
isJsonRequest = getHeader "Accept" == "application/json"

instance Controller AtticsController where
  action HomeAction = do
    renderHome

  action BandsAction = do
    if isJsonRequest
      then do
        bands <- fetchBands
        render BandsView {..}
      else renderHome

  action TopPerformancesAction {collection} = if isJsonRequest
    then do
      let n = paramOrDefault 5 "numPerformances"
      band <- fetchBand collection
      topPerformances <- fetchTopPerformances collection n
      render TopPerformancesView {..}
    else renderHome

  action ShowBandAction {collection, year} = if isJsonRequest
    then do
      band <- fetchBand collection
      performances <- fetchPerformances collection year
      render PerformancesView { .. }
    else renderHome

  action RecordingsAction {collection, date} = if isJsonRequest
    then do
      band <- fetchBand collection
      performance <- fetchPerformance collection date
      recordings <- fetchRecordings collection date
      render RecordingsView {..}
    else renderHome

  action ShowRecordingAction {identifier} = if isJsonRequest
    then do
      recording <- query @Recording |> filterWhere (#identifier, identifier) |> fetchOne
      performance@PerformanceWithMetadata{performance=Performance{bandId}}
        <- fetchPerformance' (get #performanceId recording)
      band <- fetch bandId
      songs <- query @Song |> filterWhere (#recordingId, get #id recording) |> fetch
      render ShowRecordingView {..}
    else renderHome

  action (MigrationAction _) = if isJsonRequest
    then do
      let idStr :: Text = param "identifiers"
      let identifiers = splitOn "," (cs idStr)
      items <- fetchMigrationItems identifiers
      render MigrationView { .. }
    else renderHome

