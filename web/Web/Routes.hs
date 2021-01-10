module Web.Routes where

import Data.Attoparsec.ByteString.Char8
import Data.String.Conversions (cs)
import Generated.Types
import IHP.RouterPrelude
import Web.Types

instance AutoRoute AtticsController where
  parseArgument = parseTextArgument

instance AutoRoute AppleController

instance CanRoute AppleController where
  parseRoute' = string "/.well-known/apple-app-site-association"
    >> endOfInput
    >> pure AppSiteAssociationAction