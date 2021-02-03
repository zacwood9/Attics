module Admin.View.Bands.Show where
import Admin.View.Prelude

data ShowView = ShowView { band :: Band }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BandsAction}>Bands</a></li>
                <li class="breadcrumb-item active">Show Band</li>
            </ol>
        </nav>
        <h1>Show Band</h1>
        <p>{get #name band}</p>
        <p>{get #updatedAt band}</p>
    |]
