module Admin.FrontController where

import IHP.RouterPrelude
import Admin.Controller.Prelude
import Admin.View.Layout (defaultLayout)

-- Controller Imports
import Admin.Controller.Jobs
import Admin.Controller.Bands
import Admin.Controller.Static

import IHP.AutoRefresh

instance FrontController AdminApplication where
    controllers =
        [ startPage BandsAction
        -- Generator Marker
        , parseRoute @JobsController
        , parseRoute @BandsController
        ]

instance InitControllerContext AdminApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
