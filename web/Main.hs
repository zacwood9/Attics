module Main where
import IHP.Prelude

import Config
import qualified IHP.Server
import IHP.RouterSupport
import IHP.FrameworkConfig
import IHP.Job.Types
import Web.FrontController
import Web.Types
import Admin.FrontController
import Admin.Types
import Admin.Worker

instance FrontController RootApplication where
    controllers = [
            mountFrontController WebApplication
            , mountFrontController AdminApplication
        ]

instance Worker RootApplication where
    workers _ = workers AdminApplication

main :: IO ()
main = IHP.Server.run config
