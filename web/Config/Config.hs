module Config (config) where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import System.Environment

config :: ConfigBuilder
config = do
    result <- liftIO $ lookupEnv "ATTICS_ENVIRONMENT"
    case result of
        Just "production" -> prod
        _ -> dev

dev :: ConfigBuilder
dev = do
    option Development
    option (AppHostname "localhost")

prod :: ConfigBuilder
prod = do
    option Production
    option (BaseUrl "https://attics.io")
