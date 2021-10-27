module Web.Controller.Apple where

import Web.Controller.Prelude

instance Controller AppleController  where
  action AppSiteAssociationAction = do
    renderFile "static/apple-app-site-association" "application/json"
