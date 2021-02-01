#!/usr/bin/env run-script

{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Application.Script.NightlyScrape where

import Application.Script.Prelude

run :: Script
run = do
    bands <- query @Band |> fetch
    forEach bands $ \band ->
        newRecord @NightlyScrapeJob
            |> set #bandId (get #id band)
            |> createRecord

