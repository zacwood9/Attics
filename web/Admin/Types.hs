module Admin.Types (
    module Application.Types,
    AdminApplication(..),
    StaticController(..),
    BandsController(..),
    NightlyScrapeJobController(..),
) where

import           Application.Types
import           GHC.Generics
import           Generated.Types
import           IHP.ModelSupport
import           IHP.Prelude

data AdminApplication = AdminApplication deriving (Eq, Show)
data StaticController = WelcomeAction deriving (Eq, Show, Data)

data BandsController
    = BandsAction
    | NewBandAction
    | ShowBandAction { bandId :: !(Id Band) }
    | CreateBandAction
    | EditBandAction { bandId :: !(Id Band) }
    | UpdateBandAction { bandId :: !(Id Band) }
    | DeleteBandAction { bandId :: !(Id Band) }
    deriving (Eq, Show, Data)

data NightlyScrapeJobController
    = JobsAction
    | NewJobAction { bandId :: !(Id Band) }
    | ShowJobAction { jobId :: !(Id NightlyScrapeJob) }
    deriving (Eq, Show, Data)
