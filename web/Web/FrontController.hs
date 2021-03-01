module Web.FrontController where

import IHP.RouterPrelude
-- Controller Imports

import Web.Controller.Apple
import Web.Controller.Attics
import Web.Controller.Prelude
import Web.View.Layout

instance FrontController WebApplication where
  controllers =
    [ startPage BandsAction,
      -- Generator Marker
      parseRoute @AtticsController,
      parseRoute @AppleController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
