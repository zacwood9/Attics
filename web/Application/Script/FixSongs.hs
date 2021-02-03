#!/usr/bin/env run-script
module Application.Script.FixSongs where

import Application.Script.Prelude
import Control.Monad (void)

run :: Script
run = do
    bands <- query @Band |> fetch
    forEach bands $ \band ->
        void $ newRecord @FixSongJob
            |> set #bandId (get #id band)
            |> createRecord

