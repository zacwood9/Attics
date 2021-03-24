module Admin.View.Jobs.Index where
import Admin.View.Prelude hiding (fetch)
import Admin.Types
import IHP.ControllerPrelude (fetch, FilterPrimaryKey)


data IndexView = IndexView {
    jobs :: [Include "bandId" NightlyScrapeJob],
    fixSongJobs :: [Include "bandId" FixSongJob],
    initialScrapeJobs :: [Include "bandId" InitialScrapeJob]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={JobsAction}>Jobs</a></li>
            </ol>
        </nav>

        <div class="d-flex flex-direction-row">
            <h1>Jobs
                <a href={NewJobAction} class="btn btn-primary ml-4">+ New Scrape</a>
                <a href={NewFixSongJobAction} class="btn btn-primary ml-4">+ New Fix Songs</a>
                <a href={NewInitialScrapeJobAction} class="btn btn-primary ml-4">+ New Initial Scrape</a>
            </h1>
        </div>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Band</th>
                        <th>Status</th>
                        <th>Completed At</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>
                  {forEach initialScrapeJobs renderJob''}
                  {forEach fixSongJobs renderJob'}
                  {forEach jobs renderJob}
               </tbody>
            </table>
        </div>
    |]

renderJob'' :: Include "bandId" InitialScrapeJob -> Html
renderJob'' job = let
    bandId = job |> get #bandId |> get #id
    in [hsx|
    <tr>
      <td>{job |> get #bandId |> get #name}</td>
      <td>{get #status job}</td>
      <td>{get #updatedAt job}</td>
    </tr>
|]

renderJob' :: Include "bandId" FixSongJob -> Html
renderJob' job = let
    bandId = job |> get #bandId |> get #id
    in [hsx|
    <tr>
      <td>{job |> get #bandId |> get #name}</td>
      <td>{get #status job}</td>
      <td>{get #updatedAt job}</td>
      <td><a href={ShowFixSongJobAction $ get #id job} class="text-primary">Show</a></td>
    </tr>
|]

renderJob :: Include "bandId" NightlyScrapeJob -> Html
renderJob job = let
    bandId = job |> get #bandId |> get #id
    in [hsx|
    <tr>
      <td>{job |> get #bandId |> get #name}</td>
      <td>{get #status job}</td>
      <td>{get #updatedAt job}</td>
      <td><a href={ShowJobAction $ get #id job} class="text-primary">Show</a></td>
    </tr>
|]
