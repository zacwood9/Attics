module Config
( AdminPassword(..)
, config
, test
) where

import           Data.Functor        ((<&>))
import           IHP.Environment
import           IHP.FrameworkConfig
import           IHP.Prelude
import           System.Environment

newtype AdminPassword = AdminPassword Text

config :: ConfigBuilder
config = do
    result <- liftIO $ lookupEnv "ATTICS_ENVIRONMENT"
    case result of
        Just "production" -> prod
        _                 -> dev

dev :: ConfigBuilder
dev = do
    option Development
    option (AppHostname "localhost")

prod :: ConfigBuilder
prod = do
    option Production
    option (BaseUrl "https://attics.io")

test :: ConfigBuilder
test = do
    option Development
    option (AppHostname "localhost")
