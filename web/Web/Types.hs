module Web.Types (
  module Application.Types,
  WebApplication(..),
  AtticsController(..),
  AppleController(..),
)
where

import           Application.Types
import           GHC.Generics
import           Generated.Types
import           IHP.ModelSupport
import           IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data AtticsController
  = HomeAction
  | BandsAction
  | TopPerformancesAction { collection :: !Collection }
  | ShowBandAction {collection :: !Collection, year :: !Year}
  | RecordingsAction {collection :: !Collection, date :: !Date}
  | ShowRecordingAction { identifier:: !Identifier }
  | MigrationAction { identifiers :: ![Identifier] }
  deriving (Eq, Show, Data)

data AppleController = AppSiteAssociationAction
  deriving (Eq, Show, Data)
