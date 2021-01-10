module Web.Controller.Attics where

import Web.Controller.Prelude
import Web.View.Attics.Bands
import Web.View.Attics.Performances
import Web.View.Attics.Recordings

instance Controller AtticsController where
  action BandsAction = do
    bands <- fetchBands
    render BandsView {..}

  action TopPerformancesAction {collection} = do
    let n = paramOrDefault 5 "numPerformances"
    band <- fetchBand collection
    topPerformances <- fetchTopPerformances collection n
    render TopPerformancesView {..}

  action ShowBandAction {collection, year} = do
    band <- fetchBand collection
    performances <- fetchPerformances collection year
    render PerformancesView { .. }

  action RecordingsAction {collection, date} = do
    band <- fetchBand collection
    performance <- fetchPerformance collection date
    recordings <- fetchRecordings collection date
    render RecordingsView {..}

  action ShowRecordingAction {identifier} = do
    recording <- query @Recording |> filterWhere (#identifier, identifier) |> fetchOne
    performance@PerformanceWithMetadata{performance=Performance{bandId}}
      <- fetchPerformance' (get #performanceId recording)
    band <- fetch bandId
    songs <- query @Song |> filterWhere (#recordingId, get #id recording) |> fetch
    render ShowRecordingView {..}
