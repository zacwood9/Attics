module Admin.View.Bands.New where
import Admin.View.Prelude

data NewView = NewView { band :: Band }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BandsAction}>Bands</a></li>
                <li class="breadcrumb-item active">New Band</li>
            </ol>
        </nav>
        <h1>New Band</h1>
        {renderForm band}
    |]

renderForm :: Band -> Html
renderForm band = formFor band [hsx|
    {(textField #collection)}
    {(textField #name)}
    {(textField #url)}
    {(textField #logoUrl)}
    {submitButton}
|]
