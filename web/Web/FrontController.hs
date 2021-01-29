module Web.FrontController where

import IHP.RouterPrelude
-- Controller Imports

import Web.Controller.Apple
import Web.Controller.Attics
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

instance FrontController WebApplication where
  controllers =
    [ startPage HomeAction,
      -- Generator Marker
      parseRoute @AtticsController,
      parseRoute @AppleController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
