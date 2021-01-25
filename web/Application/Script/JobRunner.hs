#!/usr/bin/env run-script
module Application.Script.JobRunner where

import Application.Script.Prelude
import Control.Monad (void)

run :: Script
run = do
    bands <- query @Band |> fetch
    mapM_ (\band ->
        newRecord @NightlyScrapeJob
          |> set #bandId (get #id band)
          |> create)
        bands