module Web.View.Attics.Performances where

import Data.Aeson
import Web.View.Prelude

data PerformancesView = PerformancesView
  { band :: Band,
    performances :: [PerformanceWithMetadata]
  }

instance View PerformancesView where
  html PerformancesView {band, performances} = error "not accesible"
  json PerformancesView {band, performances} =
    toJSON $ object ["band" .= band, "performances" .= performances]

data TopPerformancesView = TopPerformancesView
  { band :: Band,
    topPerformances :: HashMap Text [PerformanceWithMetadata]
  }

instance View TopPerformancesView where
  html TopPerformancesView {band, topPerformances} = error "not accesible"
  json TopPerformancesView {band, topPerformances} =
    toJSON $ object ["band" .= band, "top_performances" .= topPerformances]