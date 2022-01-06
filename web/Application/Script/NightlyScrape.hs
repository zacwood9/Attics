module Main where

import Application.Script.Prelude hiding ( run )
import Control.Monad (void)
import IHP.ScriptSupport ( runScript )
import qualified Config

main = runScript Config.config run

run :: Script
run = do
    bands <- query @Band |> fetch
    forEach bands $ \band ->
        void $ newRecord @NightlyScrapeJob
            |> set #bandId (get #id band)
            |> createRecord

