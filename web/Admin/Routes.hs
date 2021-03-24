module Admin.Routes where
import IHP.RouterPrelude
import Generated.Types
import Admin.Types
import Data.Proxy

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute BandsController
instance AutoRoute JobsController

