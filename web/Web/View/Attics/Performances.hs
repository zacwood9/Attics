module Web.View.Attics.Performances where

import Data.Aeson
import Web.View.Prelude
import qualified Data.HashMap.Strict as HashMap

data PerformancesView = PerformancesView
  { band :: Band,
    year :: Year,
    performances :: [PerformanceWithMetadata]
  }

instance View PerformancesView where
  html PerformancesView {band, year, performances} = [hsx|
        <div class="container mt-4">
            <div class="d-flex flex-direction-column align-items-center">
                <img src="/icon.png" width="75" height="75" class="align-center mr-2" style="border-radius: 8px" alt="">
                <span class="display-4">{get #name band}: {year}</span>
            </div>

            <section class="my-4">
                {forEach performances (renderPerformance band)}
            </section>
        </div>
  |]
  json PerformancesView {band, performances} =
    toJSON $ object ["band" .= band, "performances" .= performances]

renderPerformance band PerformanceWithMetadata { .. } =
    let
        rating' = avgRating / 5 * 100
    in [hsx|
    <a href={PlayerAction (get #collection band) (get #date performance) Nothing Nothing}>
        <div class="card mb-3">
            <div class="card-body">
                <h5 class="card-title d-flex justify-content-between">
                    {get #date performance}
                    {rating rating'}
                </h5>
                <h6 class="card-subtitle d-flex justify-content-between">
                    <span class="text-muted">{get #venue performance}, {get #city performance}, {get #state performance}</span>
                    <span class="text-muted">{numRecordings} recordings</span>
                </h6>
            </div>
        </div>
    </a>
|]

data TopPerformancesView = TopPerformancesView
  { band :: Band,
    topPerformances :: [(Year, [PerformanceWithMetadata])]
  }

instance View TopPerformancesView where
  html TopPerformancesView {band, topPerformances} = [hsx|
    <div class="container mt-4">
        <div class="d-flex flex-direction-column align-items-center">
            <img src="/icon.png" width="75" height="75" class="align-center mr-2" style="border-radius: 8px" alt="">
            <span class="display-4">{get #name band}</span>
        </div>

        <section class="my-4">
            {forEach topPerformances (renderYear band)}
        </section>
    </div>
  |]
  json TopPerformancesView {band, topPerformances} =
    toJSON $ object ["band" .= band, "top_performances" .= HashMap.fromList topPerformances]

renderYear band (year, performances) = [hsx|
    <div class="d-flex justify-content-between">
        <h4>{year}</h4>
        <a class="text-muted" href={ShowBandAction (get #collection band) year}>See all</a>
    </div>
    <div class="row my-2" style="">
        <div class="d-flex flex-wrap">
        {forEach performances (renderPerformanceSm band)}
        </div>
    </div>
    <hr/>
|]

renderPerformanceSm band PerformanceWithMetadata { .. } = [hsx|
    <a href={RecordingsAction (get #collection band) (get #date performance)}
       class="card bg-attics-blue d-flex justify-content-between px-2 py-1 mx-2 my-2"
       style="height: 8em; width: 10em"
    >
        <div>
            <div class="rating">
                {rating $ (avgRating / 5) * 100}
            </div>
        </div>
        <div>
            <span class="text-white font-weight-bold">{get #date performance}</span>
            <span class="text-lightgray d-block text-truncate">{get #venue performance}</span>
        </div>
    </a>
|]



