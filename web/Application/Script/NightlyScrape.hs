#!/usr/bin/env run-script

module Application.Script.NightlyScrape where

import Application.Script.Prelude
import Control.Monad (void)

run :: Script
run = do
    bands <- query @Band |> fetch
    forEach bands $ \band ->
        void $ newRecord @NightlyScrapeJob
            |> set #bandId (get #id band)
            |> createRecord

