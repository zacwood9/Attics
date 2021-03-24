module Web.View.Attics.Bands where

import Web.View.Prelude
import Data.Aeson

data BandsView = BandsView { bands :: [BandWithMetadata] }

instance View BandsView where
  html BandsView { bands } = [hsx|
    <div class="container mt-4">
        <div class="row">
            <div class="col-12">
                <div class="mx-auto" style="width: 100%;">
                    <div class="jumbotron">
                        <h1 class="display-5 text-center">Bands</h1>
                    </div>
                </div>
            </div>
        </div>

        <div class="row">
            {forEach bands renderBand}
        </div>
    </div>
  |]

  json BandsView { bands } = toJSON bands

renderBand :: BandWithMetadata -> Html
renderBand BandWithMetadata { .. } = [hsx|
    <div class="col-md-6 col-lg-4">
        <div class="card mx-auto" style="width: 18rem;">
            <div class="card-body">
                <h5 class="card-title">{get #name band}</h5>
                <a href={TopPerformancesAction $ get #collection band} class="btn btn-primary">View {numPerformances} Shows</a>
            </div>
        </div>
    </div>
|]
