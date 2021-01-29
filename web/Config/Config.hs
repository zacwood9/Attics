module Config (AdminPassword(..), config) where

import           Data.Functor        ((<&>))
import           IHP.Environment
import           IHP.FrameworkConfig
import           IHP.Prelude
import           System.Environment

import qualified IHP.Log as Log
import IHP.Log.Types

newtype AdminPassword = AdminPassword Text

config :: ConfigBuilder
config = do
    option Development
    logger <- liftIO $ newLogger def { formatter = withTimeAndLevelFormatter }
    option logger

    -- result <- liftIO $ lookupEnv "ATTICS_ENVIRONMENT"
    -- case result of
    --     Just "production" -> prod
    --     _                 -> dev

dev :: ConfigBuilder
dev = do
    option Development
    option (AppHostname "localhost")

prod :: ConfigBuilder
prod = do
    option Production
    option (BaseUrl "https://attics.io")
