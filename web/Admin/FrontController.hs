module Admin.FrontController where

import IHP.RouterPrelude
import Admin.Controller.Prelude
import Admin.View.Layout (defaultLayout)

-- Controller Imports
import Admin.Controller.Bands
import Admin.Controller.Static

instance FrontController AdminApplication where
    controllers =
        [ startPage BandsAction
        -- Generator Marker
        , parseRoute @BandsController
        ]

instance InitControllerContext AdminApplication where
    initContext = do
        setLayout defaultLayout
