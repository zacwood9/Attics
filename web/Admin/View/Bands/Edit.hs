module Admin.View.Bands.Edit where
import Admin.View.Prelude

data EditView = EditView { band :: Band }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BandsAction}>Bands</a></li>
                <li class="breadcrumb-item active">Edit Band</li>
            </ol>
        </nav>
        <h1>Edit Band</h1>
        {renderForm band}
    |]

renderForm :: Band -> Html
renderForm band = formFor band [hsx|
    {(textField #collection)}
    {(textField #name)}
    {(textField #url)}
    {(textField #logoUrl)}
    {(textField #updatedAt)}
    {submitButton}
|]
