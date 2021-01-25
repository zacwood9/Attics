module Web.View.Attics.Recordings where

import Data.Aeson
import Web.View.Prelude

data RecordingsView = RecordingsView
  { band :: Band,
    performance :: PerformanceWithMetadata,
    recordings :: [Recording]
  }


instance View RecordingsView where
  html RecordingsView {..} = error "not accesible"
  json RecordingsView {..} =
    toJSON $
      object
        [ "band" .= band,
          "performance" .= performance,
          "recordings" .= recordings
        ]

data ShowRecordingView = ShowRecordingView
  { band :: Band,
    performance :: PerformanceWithMetadata,
    recording :: Recording,
    songs :: [Song]
  }

instance View ShowRecordingView where
  html ShowRecordingView {..} = error "not accesible"
  json ShowRecordingView {..} =
    toJSON $
      object
        [ "band" .= band,
          "performance" .= performance,
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