#!/usr/bin/env run-script
module Application.Script.InitialScrape where

import Application.Script.Prelude
import Control.Monad (void)

run :: Script
run = do
    bands <- query @Band |> fetch
    forEach bands $ \band ->
        void $ newRecord @InitialScrapeJob
            |> set #bandId (get #id band)
            |> createRecord

