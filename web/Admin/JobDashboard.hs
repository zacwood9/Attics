{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Admin.JobDashboard where

import IHP.Prelude
import Generated.Types
import IHP.ViewPrelude (Html, View, hsx, html, timeAgo, columnNameToFieldLabel)
import IHP.ModelSupport
import IHP.ControllerPrelude
import Admin.View.Prelude (selectField, formFor', submitButton, hiddenField)
import Admin.View.Jobs.Index
import Admin.View.Jobs.Show
import Admin.View.Jobs.New
import IHP.RouterPrelude hiding (get, tshow, error, map, putStrLn)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import IHP.Job.Dashboard


-- INITIAL SCRAPE JOB
--

instance DisplayableJob InitialScrapeJob where
    makeDashboardSection = makeDashboardSectionFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makePageView = makeListPageFromTableViewable @(IncludeWrapper "bandId" InitialScrapeJob)
    makeDetailView job = do
        let table = tableName @InitialScrapeJob
        withRelated <- fetchRelated #bandId job
        pure $ SomeView $ HtmlView [hsx|
            <br>
                <h5>Viewing Job {get #id job} in {table}</h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Band</th>
                        <td>{get #bandId withRelated |> get #name}</td>
                    </tr>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job |> timeAgo}</td>
                    </tr>
                    <tr>
                        <th>Status</th>
                        <td>{renderStatus job}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>

            <div class="d-flex flex-row">
                <form class="mr-2" action="/jobs/DeleteJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="id" name="id" value={tshow $ get #id job}>
                    <button type="submit" class="btn btn-danger">Delete</button>
                </form>
                <form action="/jobs/CreateJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="bandId" name="bandId" value={get #bandId job |> tshow}>
                    <button type="submit" class="btn btn-primary">Run again</button>
                </form>
            </div>
        |]

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

instance TableViewable (IncludeWrapper "bandId" InitialScrapeJob) where
    modelTableName = tableName @InitialScrapeJob
    tableTitle = tableName @InitialScrapeJob |> columnNameToFieldLabel
    tableHeaders = ["Band", "Updated at", "Status", ""]

    getIndex =
        query @InitialScrapeJob
            |> limit 10
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    getPage page pageSize = do
        query @InitialScrapeJob
            |> offset (page * pageSize)
            |> limit pageSize
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @InitialScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
            link = "/jobs/CreateJob?tableName=" <> tableName @InitialScrapeJob  <> "&bandId=" <> get #bandId job |> get #id |> tshow
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job |> timeAgo}</td>
            <td>{renderStatus job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
            <td>
                <form action={link} method="POST">
                    <button type="submit" style={retryButtonStyle} class="btn btn-link text-secondary">Retry</button>
                </form>
            </td>
        </tr>
    |]

    newJobLink = let
            table = tableName @InitialScrapeJob
        in [hsx|
            <form action={CreateJobAction table}>
                <button type="submit" class="btn btn-primary btn-sm">+ New Job</button>
            </form>
        |]

instance DisplayableJob NightlyScrapeJob where
    makeDashboardSection = makeDashboardSectionFromTableViewable @(IncludeWrapper "bandId" NightlyScrapeJob)
    makePageView = makeListPageFromTableViewable @(IncludeWrapper "bandId" NightlyScrapeJob)
    makeDetailView job = do
        let table = tableName @NightlyScrapeJob
        withRelated <- fetchRelated #bandId job
        pure $ SomeView $ HtmlView $ [hsx|
            <br>
                <h5>Viewing Job {get #id job} in {table}</h5>
            <br>
            <table class="table">
                <tbody>
                    <tr>
                        <th>Band</th>
                        <td>{get #bandId withRelated |> get #name}</td>
                    </tr>
                    <tr>
                        <th>Updated At</th>
                        <td>{get #updatedAt job}</td>
                    </tr>
                    <tr>
                        <th>Created At</th>
                        <td>{get #createdAt job |> timeAgo}</td>
                    </tr>
                    <tr>
                        <th>Status</th>
                        <td>{renderStatus job}</td>
                    </tr>
                    <tr>
                        <th>Last Error</th>
                        <td>{fromMaybe "No error" (get #lastError job)}</td>
                    </tr>
                </tbody>
            </table>

            <div class="d-flex flex-row">
                <form class="mr-2" action="/jobs/DeleteJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="id" name="id" value={tshow $ get #id job}>
                    <button type="submit" class="btn btn-danger">Delete</button>
                </form>
                <form action="/jobs/CreateJob" method="POST">
                    <input type="hidden" id="tableName" name="tableName" value={table}>
                    <input type="hidden" id="bandId" name="bandId" value={get #bandId job |> tshow}>
                    <button type="submit" class="btn btn-primary">Run again</button>
                </form>
            </div>
        |]

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

instance TableViewable (IncludeWrapper "bandId" NightlyScrapeJob) where
    modelTableName = tableName @NightlyScrapeJob
    tableTitle = tableName @NightlyScrapeJob |> columnNameToFieldLabel
    tableHeaders = ["Band", "Updated at", "Status", ""]

    getIndex =
        query @NightlyScrapeJob
            |> limit 10
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    getPage page pageSize = do
        query @NightlyScrapeJob
            |> offset (page * pageSize)
            |> limit pageSize
            |> orderByDesc #createdAt
            |> fetch
            >>= mapM (fetchRelated #bandId)
            >>= pure . map IncludeWrapper

    renderTableRow (IncludeWrapper job) =
        let
            table = tableName @NightlyScrapeJob
            linkToView = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
            link = "/jobs/CreateJob?tableName=" <> tableName @NightlyScrapeJob  <> "&bandId=" <> get #bandId job |> get #id |> tshow
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #updatedAt job |> timeAgo}</td>
            <td>{renderStatus job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
            <td>
                <form action={link} method="POST">
                    <button type="submit" style={retryButtonStyle} class="btn btn-link text-secondary">Retry</button>
                </form>
            </td>
        </tr>
    |]

    newJobLink = let
            table = tableName @NightlyScrapeJob
        in [hsx|
            <form action={CreateJobAction table}>
                <button type="submit" class="btn btn-primary btn-sm">+ New Job</button>
            </form>
        |]

