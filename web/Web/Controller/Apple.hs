module Web.Controller.Apple where

import Web.Controller.Prelude
import Web.View.Attics.Bands
import Web.View.Attics.Performances
import Web.View.Attics.Recordings

instance Controller AppleController  where
  action AppSiteAssociationAction = do
    renderFile "static/apple-app-site-association" "application/json"