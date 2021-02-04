module Admin.View.Jobs.New where
import Admin.View.Prelude

data NewView = NewView { job :: NightlyScrapeJob, bands :: [Band] }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={JobsAction}>Jobs</a></li>
                <li class="breadcrumb-item active">New Job</li>
            </ol>
        </nav>
        <h1>New Job</h1>
        {renderForm job bands}
    |]

renderForm :: NightlyScrapeJob -> [Band] -> Html
renderForm job bands = formFor job [hsx|
    {(selectField #bandId bands)}
    {submitButton}
|]

data NewFixSongJobView = NewFixSongJobView  { job :: FixSongJob, bands :: [Band] }
instance View NewFixSongJobView  where
    html NewFixSongJobView   { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={JobsAction}>Jobs</a></li>
                <li class="breadcrumb-item active">New Job</li>
            </ol>
        </nav>
        <h1>New Job</h1>
        {renderForm' job bands}
    |]

renderForm' :: FixSongJob -> [Band] -> Html
renderForm' job bands = formFor job [hsx|
    {(selectField #bandId bands)}
    {submitButton}
|]

data NewInitialScrapeJobView = NewInitialScrapeJobView  { job :: InitialScrapeJob, bands :: [Band] }
instance View NewInitialScrapeJobView  where
    html NewInitialScrapeJobView   { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={JobsAction}>Jobs</a></li>
                <li class="breadcrumb-item active">New Job</li>
            </ol>
        </nav>
        <h1>New Job</h1>
        {renderForm'' job bands}
    |]

renderForm'' :: InitialScrapeJob -> [Band] -> Html
renderForm'' job bands = formFor job [hsx|
    {(selectField #bandId bands)}
    {submitButton}
|]

instance CanSelect Band where
    -- Here we specify that the <option> value should contain a `Id User`
    type SelectValue Band = Id Band
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
