{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module Admin.Controller.Jobs where

import Admin.Controller.Prelude
import Admin.View.Jobs.Index
import Admin.View.Jobs.Show
import Admin.Job.NightlyScrape

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

instance Controller NightlyScrapeJobController where
    action JobsAction = do
        result <- query @NightlyScrapeJob
            |> orderByDesc #updatedAt
            |> fetch
            >>= collectionFetchRelated #bandId

        render $ IndexView result

    action NewJobAction { bandId } = do
        newRecord @NightlyScrapeJob
            |> set #bandId bandId
            |> create
        redirectTo JobsAction

    action ShowJobAction { jobId } = do
        job <- fetch jobId >>= fetchRelated #bandId
        render JobView { job }

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

--     action CreateJobAction = do
--         let job = newRecord @Job
--         job
--             |> buildJob
--             |> ifValid \case
--                 Left job -> render NewView { .. }
--                 Right job -> do
--                     job <- job |> createRecord
--                     setSuccessMessage "Job created"
--                     redirectTo JobsAction

--     action DeleteJobAction { jobId } = do
--         job <- fetch jobId
--         deleteRecord job
--         setSuccessMessage "Job deleted"
--         redirectTo JobsAction

-- buildJob job = job
--     |> fill @'[]
