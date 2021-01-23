module Admin.View.Bands.Index where

import Admin.View.Prelude

data IndexView = IndexView {bands :: [BandWithMetadata]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={BandsAction}>Bands</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewBandAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Band</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach bands renderBand}</tbody>
            </table>
        </div>
    |]

renderBand band =
  let band' = get #band band
   in [hsx|
    <tr>
        <td>
            {get #name band'} ({get #numPerformances band} performances)
        </td>
        <td><a href={ShowBandAction (get #id band')}>Show</a></td>
        <td><a href={EditBandAction (get #id band')} class="text-muted">Edit</a></td>
        <td><a href={DeleteBandAction (get #id band')} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
