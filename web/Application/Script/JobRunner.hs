module Main (main) where

import Application.Script.Prelude
import IHP.ScriptSupport
import IHP.Job.Runner
import qualified Config
import RootApplication ()

main :: IO ()
main = runScript Config.config (runJobWorkers (workers RootApplication))
