module Admin.View.Jobs.Show where
import Admin.View.Prelude

data JobView = JobView { job :: Include "bandId" NightlyScrapeJob }

instance View JobView where
    html JobView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={JobsAction}>Jobs</a></li>
                <li class="breadcrumb-item active">Show Job</li>
            </ol>
        </nav>
        <h1>Nightly Scrape: {job |> get #bandId |> get #name}</h1>
        <div class="table-responsive">
            <table class="table">
                <tbody>
                {job |> get #bandId |> get #name |> renderRow "Band"}
                {job |> get #status |> show |> renderRow "Status"}
                {job |> get #updatedAt |> show |> renderRow "Updated at"}
                {job |> get #lastError |> show |> renderRow "Error"}
                </tbody>
            </table>
        </div>
        <form action={job |> get #bandId |> get #id |> NewJobAction}>
            <button class="btn btn-primary" type="submit">Run new job</button>
        </form>
    |]

renderRow :: Text -> Text -> Html
renderRow name value = [hsx|
  <tr>
    <td>
      <strong>{name}</strong>
    </td>
    <td>
      {value}
    </td>
  </tr>
|]
