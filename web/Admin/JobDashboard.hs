{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Admin.JobDashboard (
    JobsDashboard(..),
    DisplayableJob(..),
    GenericJobDashboardSection(..),
    JobDashboardSection(..),
    GenericJobsController(..),
    GenericIndexView(..),
) where

import IHP.Prelude
import Generated.Types
import IHP.ViewPrelude (Html, View, hsx, html)
import IHP.ModelSupport
import IHP.ControllerPrelude
import Admin.View.Jobs.Index
import Admin.View.Jobs.Show
import Admin.View.Jobs.New
import Unsafe.Coerce
import IHP.RouterPrelude hiding (get, tshow, error)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

data GenericJobsController (jobs :: [*])
    = AllJobsAction
    | ViewJobAction
    deriving (Show, Eq, Data)

data JobDashboardSection job = JobDashboardSection {
    title :: Text,
    headerRows :: [Text],
    jobs:: [job]
}

data GenericJobDashboardSection =
    forall job. (DisplayableJob job) => GenericJobDashboardSection (JobDashboardSection job)

class JobsDashboard (jobs :: [*]) where
    makeDashboard :: (?modelContext :: ModelContext) => IO SomeView
    indexPage :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    viewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

instance JobsDashboard '[] where
    makeDashboard = pure (SomeView EmptyView)
    indexPage = pure ()
    viewJob = pure ()

data SomeView = forall a. (View a) => SomeView a
instance View SomeView where
    html (SomeView a) = let ?view = a in IHP.ViewPrelude.html a

instance (View a) => View [a] where
    html [] = [hsx||]
    html (x:xs) =
        let ?view = x in
            let current = IHP.ViewPrelude.html x in
                let ?view = xs in
                    let rest = IHP.ViewPrelude.html xs in
                        [hsx|{current}{rest}|]

data EmptyView = EmptyView
instance View EmptyView where
    html _ = [hsx||]


instance {-# OVERLAPPABLE #-} (DisplayableJob job, JobsDashboard rest) => JobsDashboard (job:rest) where
    makeDashboard = do
        section <- makeSection @job
        restSections <- SomeView <$> makeDashboard @rest
        pure $ SomeView (section : [restSections])

    indexPage = do
        dashboard <- makeDashboard @(job:rest)
        render dashboard

    viewJob = do
        let table = param "tableName"
        if tableName @job == table
            then do
                let id :: Id job = unsafeCoerce (param "id" :: UUID) -- TODO: safe cast?
                j <- fetch id
                render $ GenericShowView j
            else do
                viewJob @rest

class ( job ~ GetModelByTableName (GetTableName job)
    , FilterPrimaryKey (GetTableName job)
    , FromRow job
    , Show (PrimaryKey (GetTableName job))
    , PG.FromField (PrimaryKey (GetTableName job))
    , KnownSymbol (GetTableName job)
    , HasField "id" job (Id job)
    , HasField "status" job JobStatus
    , HasField "updatedAt" job UTCTime
    , CanUpdate job
    , Show job
    , Eq job
    , Typeable job
    ) => DisplayableJob job where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        let name = tableName @job
        header <- sectionHeader @job
        jobs <- query @job |> fetch
        pure $ SomeView (GenericJobDashboardSection (JobDashboardSection name header jobs))

    -- viewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

    sectionHeader :: (?modelContext :: ModelContext) => IO [Text]
    sectionHeader = do
       pure ["ID", "Status", "Time updated", ""]

    renderJobRow :: job -> Html
    renderJobRow job =
        let
            table = tableName @job
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
            <tr>
                <td>{get #id job}</td>
                <td>{get #status job}</td>
                <td>{get #updatedAt job}</td>
                <td><a href={linkToView} class="text-primary">Show</a></td>
            </tr>
        |]

instance DisplayableJob InitialScrapeJob where
    makeSection :: (?modelContext :: ModelContext) => IO SomeView
    makeSection = do
        jobsWithBand <- query @InitialScrapeJob
            |> fetch
            >>= mapM (fetchRelated #bandId)
        pure (SomeView (TableView jobsWithBand))

data TableView = forall a. (TableViewable a) => TableView [a]
instance View TableView where
    html (TableView rows) = renderTable rows

class TableViewable a where
    tableTitle :: Text
    tableHeaders :: [Text]
    renderTableRow :: a -> Html

    renderTable :: [a] -> Html
    renderTable rows =
        let
            title = tableTitle @a
            headers = tableHeaders @a
            renderRow = renderTableRow @a
        in [hsx|
        <div>
            <h3>Job type: {title}</h3>
            <table class="table">
                <thead>
                    <tr>
                        {forEach headers renderHeader}
                    </tr>
                </thead>

                <tbody>
                    {forEach rows renderRow}
                </tbody>
            </table>
        </div>
    |]
        where renderHeader field = [hsx|<th>{field}</th>|]

instance {-# OVERLAPPABLE #-} (job ~ Include "bandId" InitialScrapeJob) => TableViewable job where
    tableTitle = "Initial Scrape Job"
    tableHeaders = ["Band", "Status", "Updated at", ""]
    renderTableRow job =
        let
            table = tableName @InitialScrapeJob
            linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
        in [hsx|
        <tr>
            <td>{job |> get #bandId |> get #name}</td>
            <td>{get #status job}</td>
            <td>{get #updatedAt job}</td>
            <td><a href={linkToView} class="text-primary">Show</a></td>
        </tr>
    |]

instance DisplayableJob NightlyScrapeJob
instance DisplayableJob FixSongJob

 -- CONTROLLER
instance (JobsDashboard jobs) => Controller (GenericJobsController jobs) where
    action AllJobsAction = do
        indexPage @(jobs)

    action ViewJobAction = do
        viewJob @(jobs)

data GenericShowView = forall job. (DisplayableJob job) => GenericShowView job

-- VIEW
getTableName :: forall job. (DisplayableJob job) => job -> Text
getTableName _ = tableName @job

instance View GenericShowView where
    html (GenericShowView job) =
        let
            table = getTableName job
        in [hsx|
            <h3>Viewing {table}</h3>

            {get #id job}
        |]

data GenericIndexView = GenericIndexView { sections :: [GenericJobDashboardSection] }

instance View GenericIndexView where
    html GenericIndexView { .. } = [hsx|
        <div>
            {forEach sections renderGenericJobSection}
        </div>
    |]

instance View GenericJobDashboardSection where
    html a = [hsx|{renderGenericJobSection a}|]


renderGenericJobSection :: GenericJobDashboardSection -> Html
renderGenericJobSection (GenericJobDashboardSection (JobDashboardSection { .. })) = [hsx|
    <div>
        <h3>Job type: {title}</h3>
        <table class="table">
            <thead>
                <tr>
                    {forEach headerRows renderHeader}
                </tr>
            </thead>

            <tbody>
                {forEach jobs renderJobRow}
            </tbody>
        </table>
    </div>
|]
    where renderHeader field = [hsx|<th>{field}</th>|]


instance HasPath (GenericJobsController jobs) where
    pathTo AllJobsAction = "/GenericJobs"

instance CanRoute (GenericJobsController jobs) where
    parseRoute' = do
        (string "/jobs/ListJobs" <* endOfInput >> pure AllJobsAction)
        <|> (string "/jobs/ViewJob" <* endOfInput >> pure ViewJobAction)

