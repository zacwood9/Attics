module Web.View.Attics.Bands where

import Web.View.Prelude
import Data.Aeson

data BandsView = BandsView {bands :: [BandWithMetadata ]}

instance View BandsView where
  html BandsView { .. } = comingSoon

  json BandsView { bands } = toJSON bands
