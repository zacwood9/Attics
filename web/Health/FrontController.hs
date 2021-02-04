module Health.FrontController where

import IHP.RouterPrelude
import Health.Controller.Prelude
import Health.View.Layout (defaultLayout)

-- Controller Imports
import Health.Controller.Static

instance FrontController HealthApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        ]

instance InitControllerContext HealthApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
