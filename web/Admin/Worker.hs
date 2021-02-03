module Admin.Worker where

import IHP.Prelude
import Admin.Types
import Generated.Types
import IHP.Job.Runner
import IHP.Job.Types

import Admin.Job.NightlyScrape
import Admin.Job.FixSong

instance Worker AdminApplication where
    workers _ =
        [ worker @FixSongJob,
          worker @NightlyScrapeJob
        -- Generator Marker
        ]
