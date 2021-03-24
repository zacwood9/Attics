{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Admin.JobDashboard where

import IHP.Prelude
import Generated.Types
import IHP.ViewPrelude (Html, View, hsx, html)
import IHP.ModelSupport
import IHP.ControllerPrelude
import Admin.View.Prelude (selectField, formFor', submitButton, hiddenField)
import Admin.View.Jobs.Index
import Admin.View.Jobs.Show
import Admin.View.Jobs.New
import Unsafe.Coerce
import IHP.RouterPrelude hiding (get, tshow, error, map)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import IHP.Job.Dashboard

-- INITIAL SCRAPE JOB
--
instance TableViewable (IncludeWrapper "bandId" InitialScrapeJob) where
    tableTitle = "Initial Scrape Job"
    tableHeaders = ["Band", "Updated at", "Status", ""]
    createNewForm = newJobFormForTableHeader @InitialScrapeJob
    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @InitialScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job}</td>
            <td>{statusToBadge $ get #status job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
        </tr>
    |]


newtype HtmlView = HtmlView Html
instance View HtmlView where
    html (HtmlView html) = [hsx|{html}|]

instance {-# OVERLAPS #-} DisplayableJob InitialScrapeJob where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        jobsWithBand <- query @InitialScrapeJob
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map (IncludeWrapper @"bandId" @InitialScrapeJob)
        pure (SomeView (TableView jobsWithBand))

    makeDetailView :: (?modelContext :: ModelContext) => InitialScrapeJob -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ InitialScrapeJobForm job

    makeNewJobView = do
        bands <- query @Band |> fetch
        pure $ SomeView $ HtmlView $ form newRecord bands
        where
            form :: InitialScrapeJob -> [Band] -> Html
            form job bands = formFor' job "/jobs/CreateJob" [hsx|
                {selectField #bandId bands}
                <input type="hidden" id="tableName" name="tableName" value={getTableName job}>
                <button type="submit" class="btn btn-primary">Run again</button>
            |]


    createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    createNewJob = do
        let bandId = param "bandId"
        newRecord @InitialScrapeJob |> set #bandId bandId |> create
        pure ()

data InitialScrapeJobForm = InitialScrapeJobForm InitialScrapeJob
instance View InitialScrapeJobForm where
    html (InitialScrapeJobForm job) =
        let
            table = getTableName job
        in [hsx|
            <br>
                <h5>Viewing Job {get #id job} in </h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>
            <form action="/jobs/CreateJob" method="POST">
                <input type="hidden" id="tableName" name="tableName" value={table}>
                <input type="hidden" id="bandId" name="bandId" value={tshow $ get #bandId job}>
                <button type="submit" class="btn btn-primary">Run again</button>
            </form>
        |]


-- NIGHTLY SCRAPE JOB
instance TableViewable (IncludeWrapper "bandId" NightlyScrapeJob) where
    tableTitle = "Nightly Scrape Job"
    tableHeaders = ["Band", "Updated at", "Status", ""]
    createNewForm = newJobFormForTableHeader @NightlyScrapeJob
    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @NightlyScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job}</td>
            <td>{statusToBadge $ get #status job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
        </tr>
    |]

instance DisplayableJob NightlyScrapeJob where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        jobsWithBand <- query @NightlyScrapeJob
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map (IncludeWrapper @"bandId" @NightlyScrapeJob)
        pure (SomeView (TableView jobsWithBand))

    makeDetailView :: (?modelContext :: ModelContext) => NightlyScrapeJob -> IO SomeView
    makeDetailView job = do
        pure $ SomeView $ NightlyScrapeJobForm job

    makeNewJobView = do
        bands <- query @Band |> fetch
        pure $ SomeView $ HtmlView $ form newRecord bands
        where
            form :: NightlyScrapeJob -> [Band] -> Html
            form job bands = formFor' job "/jobs/CreateJob" [hsx|
                {selectField #bandId bands}
                <input type="hidden" id="tableName" name="tableName" value={getTableName job}>
                <button type="submit" class="btn btn-primary">Run again</button>
            |]

    createNewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    createNewJob = do
        let bandId = param "bandId"
        newRecord @NightlyScrapeJob |> set #bandId bandId |> create
        pure ()

data NightlyScrapeJobForm = NightlyScrapeJobForm NightlyScrapeJob
instance View NightlyScrapeJobForm   where
    html (NightlyScrapeJobForm job) =
        let
            table = getTableName job
        in [hsx|
            <br>
                <h5>Viewing Job {get #id job} in </h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>
            <form action="/jobs/CreateJob" method="POST">
                <input type="hidden" id="tableName" name="tableName" value={table}>
                <input type="hidden" id="bandId" name="bandId" value={tshow $ get #bandId job}>
                <button type="submit" class="btn btn-primary">Run again</button>
            </form>
        |]
