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
import IHP.RouterPrelude hiding (get, tshow)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

data GenericJobsController (jobs :: [*])
    =  AllJobsAction
    | ViewJobAction
    deriving (Show, Eq, Data)

data JobDashboardSection job = JobDashboardSection {
    title :: Text,
    jobs:: [job]
}

data GenericJobDashboardSection =
    forall job. (DisplayableJob job) => GenericJobDashboardSection (JobDashboardSection job)

class JobsDashboard (jobs :: [*]) where
    makeDashboard :: (?modelContext :: ModelContext) => IO [GenericJobDashboardSection]
    indexPage :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()
    viewJob :: (?context::ControllerContext, ?modelContext::ModelContext) => IO ()

instance JobsDashboard '[] where
    makeDashboard = pure []
    indexPage = pure ()
    viewJob = pure ()

instance {-# OVERLAPPABLE #-} (DisplayableJob job, JobsDashboard rest) => JobsDashboard (job:rest) where
    makeDashboard = do
        section <- makeSection @job
        let generic = GenericJobDashboardSection section
        restSections <- makeDashboard @rest
        pure (generic : restSections)

    indexPage = do
        dashboard <- makeDashboard @(job:rest)
        render $ GenericIndexView dashboard

    viewJob = do
        let table = param "tableName"
        if tableName @job == table
            then do
                let id :: Id job = unsafeCoerce (param "id" :: UUID) -- TODO: safe cast?
                j <- fetch id
                render $ GenericShowView j
            else do
                viewJob @rest

instance (JobsDashboard rest) => JobsDashboard (InitialScrapeJob:rest) where
    makeDashboard = do
        section <- makeSection @InitialScrapeJob
        let generic = GenericJobDashboardSection section
        restSections <- makeDashboard @rest
        pure (generic : restSections)

    indexPage = do
        dashboard <- makeDashboard @(InitialScrapeJob:rest)
        render $ GenericIndexView dashboard

    viewJob = do
        let table = param "tableName"
        if tableName @InitialScrapeJob == table
            then do
                let id :: Id InitialScrapeJob = unsafeCoerce (param "id" :: UUID) -- TODO: safe cast?
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
    makeSection :: (?modelContext :: ModelContext) => IO (JobDashboardSection job)
    makeSection = do
        let name = tableName @job
        jobs <- query @job |> fetch
        pure (JobDashboardSection name jobs)

instance DisplayableJob InitialScrapeJob
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

renderGenericJobSection :: GenericJobDashboardSection -> Html
renderGenericJobSection (GenericJobDashboardSection (JobDashboardSection title jobs)) = [hsx|
    <div>
        <h3>Job type: {title}</h3>
        <table class="table">
            <thead>
                <tr>
                    <th>ID</th>
                    <th>Status</th>
                    <th>Time updated</th>
                    <th></th>
                </tr>
            </thead>

            <tbody>
                {forEach jobs renderGenericJob}
            </tbody>
        </table>
    </div>
|]

renderGenericJob :: forall job. (DisplayableJob job) => job -> Html
renderGenericJob job =
    let
        table = tableName @job
        linkToView :: Text = "/jobs/ViewJob?tableName=" <> table <> "&id=" <> tshow (get #id job)
    in [hsx|
        <tr>
            <td>{get #id job}</td>
            <td>{get #status job}</td>
            <td>{get #updatedAt job}</td>
            <td><a href={linkToView} class="text-muted">View</a></td>
        </tr>
    |]

instance HasPath (GenericJobsController jobs) where
    pathTo AllJobsAction = "/GenericJobs"

instance CanRoute (GenericJobsController jobs) where
    parseRoute' = do
        (string "/jobs/ListJobs" <* endOfInput >> pure AllJobsAction)
        <|> (string "/jobs/ViewJob" <* endOfInput >> pure ViewJobAction)

