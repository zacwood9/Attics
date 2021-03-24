module Admin.FrontController where

import IHP.RouterPrelude
import Admin.Controller.Prelude
import Admin.View.Layout (defaultLayout)

-- Controller Imports
import Admin.Controller.Jobs
import Admin.Controller.Bands
import Admin.Controller.Static

import IHP.AutoRefresh

import IHP.Job.Dashboard
import Admin.JobDashboard

type AtticsJobDashboard = JobsDashboardController [InitialScrapeJob, NightlyScrapeJob, FixSongJob]
instance FrontController AdminApplication where
    controllers =
        [ startPage BandsAction
        -- Generator Marker
        , parseRoute @BandsController
        , parseRoute @AtticsJobDashboard
        ]

instance InitControllerContext AdminApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
