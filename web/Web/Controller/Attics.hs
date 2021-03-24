module Web.Controller.Attics where

import Application.Helper.Queries
import Web.Controller.Prelude
import Web.View.Attics.Bands
import Web.View.Attics.Performances
import Web.View.Attics.Recordings
import Web.View.Attics.Player
import qualified Data.Text as Text
import Web.View.Prelude (HomeView (..))
import Web.View.Layout
import qualified Data.HashMap.Strict as HashMap
import qualified IHP.Log as Log
import qualified Data.List as List

renderHome :: _ => IO ()
renderHome = do
    bands <- query @Band |> fetchCount
    shows <- query @Performance |> fetchCount
    render HomeView { .. }

isJsonRequest :: _ => Bool
isJsonRequest = getHeader "Accept" == "application/json"

instance Controller AtticsController where
    beforeAction = do
        playerStates <- mapM getSession ["title", "track", "date", "bandName", "bandCollection", "venue"]
        if List.length playerStates == List.length (catMaybes playerStates)
            then do
                let s = catMaybes playerStates
                putContext $ PlayerState {
                    songTitle = s !! 0,
                    track = s !! 1,
                    date = s !! 2,
                    bandName = s !! 3,
                    bandCollection = s !! 4,
                    venue = s !! 5
                }
            else
                pure ()

    action HomeAction = do
        setLayout homeLayout
        renderHome

    action BandsAction = do
        bands <- fetchBands
        render BandsView {..}

    action TopPerformancesAction {collection} = do
        let n = paramOrDefault 5 "numPerformances"
        band <- fetchBandByCollection collection
        topPerformances <- sortBy (\a b -> fst a `compare` fst b) . HashMap.toList <$> fetchTopPerformances collection n
        render TopPerformancesView {..}

    action ShowBandAction {collection, year} = do
        band <- fetchBandByCollection collection
        performances <- fetchPerformances collection year
        render PerformancesView { .. }

    action RecordingsAction {collection, date} = do
        unless isJsonRequest (redirectTo (PlayerAction collection date Nothing Nothing))
        band <- fetchBandByCollection collection
        performance <- fetchPerformance collection date
        recordings <- fetchRecordings collection date
        render RecordingsView { .. }

    action ShowRecordingAction {identifier} = if isJsonRequest
        then do
        recording <- query @Recording |> filterWhere (#identifier, identifier) |> fetchOne
        performanceWithMetadata <- fetchPerformanceWithMetadataFromId (get #performanceId recording)
        band <- performanceWithMetadata |> get #performance |> get #bandId |> fetch
        songs <- query @Song |> filterWhere (#recordingId, get #id recording) |> fetch
        render ShowRecordingView { .. }
        else renderHome

    action (MigrationAction _) = if isJsonRequest
        then do
        let idStr :: Text = param "identifiers"
        let identifiers = Text.splitOn "," (cs idStr)
        items <- fetchMigrationItems identifiers
        render MigrationView { .. }
        else renderHome

    action PlayerAction { .. } = do
        band <- fetchBandByCollection collection
        performance <- query @Performance
            |> filterWhere (#bandId, get #id band)
            |> filterWhere (#date, date)
            |> fetchOne
        recordings <- fetchRecordings collection date
        let selectedRecording = fromMaybe (recordings !! 0) $ do
            id <- selectedIdentifier
            let list = filter (\r -> (get #identifier r) == id) recordings
            head list
        songs <- query @Song |> filterWhere (#recordingId, get #id selectedRecording) |> orderByAsc #track |> fetch

        case selectedTrack of
            -- Just track -> putContext $ SongTitle $ get #title (songs !! track)
            Just track -> do
                setSession "title" (get #title $ songs !! (track - 1))
                setSession "track" (tshow track)
                setSession "date" (get #date performance)
                setSession "bandName" (get #name band)
                setSession "bandCollection" (get #collection band)
                setSession "venue" (get #venue performance)
            Nothing -> pure ()

        playerStates <- mapM getSession ["title", "track", "date", "bandName", "bandCollection", "venue"]
        if List.length playerStates == List.length (catMaybes playerStates)
            then do
                let s = catMaybes playerStates
                putContext $ PlayerState {
                    songTitle = s !! 0,
                    track = s !! 1,
                    date = s !! 2,
                    bandName = s !! 3,
                    bandCollection = s !! 4,
                    venue = s !! 5
                }
            else
                pure ()

        render PlayerView { .. }

