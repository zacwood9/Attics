{-# LANGUAGE GADTs #-}

module Admin.View.Jobs.Index where
import Admin.View.Prelude hiding (fetch)
import IHP.ControllerPrelude (fetch, FilterPrimaryKey)

data IndexView = IndexView { jobs :: [Include "bandId" NightlyScrapeJob] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={JobsAction}>Jobs</a></li>
            </ol>
        </nav>
        <h1>Nightly Scrape Jobs</h1>
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
                 <tr>
                  {forEach jobs renderJob}
                 </tr>
               </tbody>
            </table>
        </div>
    |]

renderJob :: Include "bandId" NightlyScrapeJob -> Html
renderJob job = let
    bandId = job |> get #bandId |> get #id
    in [hsx|
    <tr>
      <td>{job |> get #bandId |> get #name}</td>
      <td>{get #status job}</td>
      <td>{get #updatedAt job}</td>
      <td><a href={ShowJobAction $ get #id job} class="text-muted">Show</a></td>
      <td><a href={NewJobAction bandId}>Retry</a></td>
    </tr>
|]
