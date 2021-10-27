module Web.View.Attics.Shows where
import Web.View.Prelude

data ShowsView = ShowsView {
    collection :: Collection,
    year :: Year
}

instance View ShowsView where
    html ShowsView { .. } = [hsx|
        <h1>ShowsView</h1>
    |]
