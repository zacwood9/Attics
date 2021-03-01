module Application.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import GHC.Generics

type Collection = Text
type Date = Text
type Identifier = Text
type Year = Text

data PerformanceWithMetadata = PerformanceWithMetadata {
  performance :: Performance,
  avgRating :: Double,
  numReviews :: Int,
  numRecordings :: Int,
  archiveDownloads :: Int,
  atticsDownloads :: Int
} deriving (Generic, Show)

data BandWithMetadata = BandWithMetadata {
  band :: Band,
  numPerformances :: Int,
  numRecordings :: Int
} deriving (Generic, Show)

data MigrationItem = MigrationItem BandWithMetadata PerformanceWithMetadata Recording
  deriving (Generic)
