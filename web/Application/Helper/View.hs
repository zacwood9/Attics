module Application.Helper.View
  ( -- To use the built in login:
    -- module IHP.LoginSupport.Helper.View
    toJSON,
  )
where

-- Here you can add functions which are available in all your views

-- To use the built in login:
-- import IHP.LoginSupport.Helper.View

import Data.Aeson
import Generated.Types
import IHP.ViewPrelude
import Web.Types

instance ToJSON Band where
  toJSON Band {..} =
    object
      [ "id" .= id,
        "collection" .= collection,
        "name" .= name,
        "updated_at" .= updatedAt,
        "url" .= url,
        "logo_url" .= logoUrl
      ]

instance ToJSON PerformanceWithMetadata where
  toJSON PerformanceWithMetadata {performance = Performance {..}, ..} =
    object
      [ "id" .= id,
        "band_id" .= bandId,
        "date" .= date,
        "venue" .= venue,
        "city" .= city,
        "state" .= state,
        "avg_rating" .= avgRating,
        "num_reviews" .= numReviews,
        "num_recordings" .= numRecordings,
        "archive_downloads" .= archiveDownloads,
        "attics_downloads" .= atticsDownloads
      ]


instance ToJSON BandWithMetadata where
  toJSON BandWithMetadata {..} =
    let (Object bandMap) = toJSON band
        (Object meta) =
          object
            [ "num_performances" .= numPerformances,
              "num_recordings" .= numRecordings
            ]
     in Object (bandMap <> meta)

instance ToJSON Recording where
  toJSON Recording {..} =
    object
      [ "id" .= id,
        "performance_id" .= performanceId,
        "transferer" .= transferer,
        "source" .= source,
        "lineage" .= lineage,
        "archive_downloads" .= archiveDownloads,
        "attics_downloads" .= atticsDownloads,
        "avg_rating" .= avgRating,
        "num_reviews" .= numReviews,
        "identifier" .= identifier
      ]

instance ToJSON Song where
  toJSON Song {..} =
    object
      [ "id" .= id,
        "file_name" .= fileName,
        "track" .= track,
        "title" .= title,
        "creator" .= creator,
        "length" .= length,
        "album" .= album,
        "recording_id" .= recordingId
      ]