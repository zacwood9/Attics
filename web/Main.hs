module Main where

import IHP.Prelude
import Config
import qualified IHP.Server
import RootApplication

main :: IO ()
main = IHP.Server.run config
