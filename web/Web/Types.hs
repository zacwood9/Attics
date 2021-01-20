module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import GHC.Generics

data WebApplication = WebApplication deriving (Eq, Show)

data AtticsController
  = BandsAction
  | TopPerformancesAction { collection :: !Collection }
  | ShowBandAction {collection :: !Collection, year :: !Year}
  | RecordingsAction {collection :: !Collection, date :: !Date}
  | ShowRecordingAction { identifier:: !Identifier }
  | MigrationAction { identifiers :: ![Identifier] }
  deriving (Eq, Show, Data)

data AppleController = AppSiteAssociationAction
  deriving (Eq, Show, Data)

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
} deriving (Generic)

data BandWithMetadata = BandWithMetadata {
  band :: Band,
  numPerformances :: Int,
  numRecordings :: Int
} deriving (Generic)

data MigrationItem = MigrationItem BandWithMetadata PerformanceWithMetadata Recording
  deriving (Generic)