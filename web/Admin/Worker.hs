module Admin.Worker where

import IHP.Prelude
import Admin.Types
import Generated.Types
import IHP.Job.Runner
import IHP.Job.Types

import Admin.Job.NightlyScrape

instance Worker AdminApplication where
    workers _ =
        [ worker @NightlyScrapeJob
        -- Generator Marker
        ]
