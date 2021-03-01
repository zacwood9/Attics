{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module Admin.Controller.Jobs where

import Admin.Controller.Prelude
import Admin.View.Jobs.Index
import Admin.View.Jobs.Show
import Admin.View.Jobs.New
import Admin.Job.NightlyScrape

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

instance Controller JobsController where
    action JobsAction = autoRefresh do
        result <- query @NightlyScrapeJob
            |> orderByDesc #updatedAt
            |> fetch
            >>= collectionFetchRelated #bandId

        fixSongs <- query @FixSongJob
            |> orderByDesc #updatedAt
            |> fetch
            >>= collectionFetchRelated #bandId

        initialScrape <- query @InitialScrapeJob
            |> orderByDesc #updatedAt
            |> fetch
            >>= collectionFetchRelated #bandId

        render $ IndexView result fixSongs initialScrape

    action NewJobAction = do
        let job = newRecord
        bands <- query @Band |> fetch
        render NewView { .. }

    action NewFixSongJobAction = do
        let job = newRecord
        bands <- query @Band |> fetch
        render NewFixSongJobView { .. }

    action NewInitialScrapeJobAction = do
        let job = newRecord
        bands <- query @Band |> fetch
        render NewInitialScrapeJobView { .. }

    action ShowJobAction { jobId } = do
        job <- fetch jobId >>= fetchRelated #bandId
        render JobView { job }

    action ShowFixSongJobAction { fixSongJobId } = do
        job <- fetch fixSongJobId >>= fetchRelated #bandId
        render FixSongJobView { job }

    action ShowInitialScrapeJobAction { initialScrapeJobId } = do
        job <- fetch initialScrapeJobId  >>= fetchRelated #bandId
        render InitialScrapeJobView { job }

    action CreateNightlyScrapeJobAction = do
        let job = newRecord @NightlyScrapeJob
        job
            |> buildJob
            |> ifValid \case
                Left job -> redirectTo NewJobAction
                Right job -> do
                    job <- job |> createRecord
                    setSuccessMessage "Job created"
                    redirectTo JobsAction

    action CreateFixSongJobAction = do
        let job = newRecord @FixSongJob
        job
            |> buildJob
            |> ifValid \case
                Left job -> redirectTo NewJobAction
                Right job -> do
                    job <- job |> createRecord
                    setSuccessMessage "Job created"
                    redirectTo JobsAction

    action CreateInitialScrapeJobAction = do
        let job = newRecord @InitialScrapeJob
        job
            |> buildJob
            |> ifValid \case
                Left job -> redirectTo NewJobAction
                Right job -> do
                    job <- job |> createRecord
                    setSuccessMessage "Job created"
                    redirectTo JobsAction



--     action EditJobAction { jobId } = do
--         job <- fetch jobId
--         render EditView { .. }

--     action UpdateJobAction { jobId } = do
--         job <- fetch jobId
--         job
--             |> buildJob
--             |> ifValid \case
--                 Left job -> render EditView { .. }
--                 Right job -> do
--                     job <- job |> updateRecord
--                     setSuccessMessage "Job updated"
--                     redirectTo EditJobAction { .. }


--     action DeleteJobAction { jobId } = do
--         job <- fetch jobId
--         deleteRecord job
--         setSuccessMessage "Job deleted"
--         redirectTo JobsAction

buildJob job = job
    |> fill @'["bandId"]
