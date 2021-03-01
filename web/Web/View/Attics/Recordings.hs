module Web.View.Attics.Recordings where

import Data.Aeson
import Web.View.Prelude

data RecordingsView = RecordingsView
  { band :: Band,
    performance :: PerformanceWithMetadata,
    recordings :: [Recording]
  }


instance View RecordingsView where
  html RecordingsView { .. } = let
    date = performance |> get #performance |> get #date
    in [hsx|
    <div class="container mt-4">
        <span class="display-4">{date <> " Recordings"}</span>

        <section class="my-4">
            {forEach recordings renderRecording}
        </section>
    </div>
  |]
  json RecordingsView {..} =
    toJSON $
      object
        [ "band" .= band,
          "performance" .= performance,
          "recordings" .= recordings
        ]


renderRecording recording = [hsx|
    <a href=""
       class="card bg-attics-blue d-flex justify-content-between px-2 py-1 mx-2 my-2"
       style="height: 8em; width: 20em"
    >
        <div>
            <div class="rating">
                {rating $ get #avgRating recording}
            </div>
        </div>
        <div>
            <span class="text-white font-weight-bold">{get #transferer recording}</span>
        </div>
    </a>
|]

data ShowRecordingView = ShowRecordingView
  { band :: Band,
    performanceWithMetadata :: PerformanceWithMetadata,
    recording :: Recording,
    songs :: [Song]
  }

instance View ShowRecordingView where
  html ShowRecordingView {..} = error "not accesible"
  json ShowRecordingView {..} =
    toJSON $
      object
        [ "band" .= band,
          "performance" .= performanceWithMetadata,
          "recording" .= recording,
          "songs" .= songs
        ]

newtype MigrationView = MigrationView
  { items :: [MigrationItem] }

instance View MigrationView where
  html MigrationView { .. } = error "not accesible"
  json MigrationView { .. } = toJSON items

instance ToJSON MigrationItem where
  toJSON (MigrationItem band performance recording) = object [
    "band" .= band,
    "performance" .= performance,
    "recording" .= recording
    ]
