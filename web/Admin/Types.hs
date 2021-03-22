{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Admin.Types (
    module Application.Types,
    AdminApplication(..),
    StaticController(..),
    BandsController(..),
    JobsController(..),
) where

import           Application.Types
import           GHC.Generics
import           Generated.Types
import           IHP.ModelSupport
import           IHP.Prelude
import           IHP.ControllerPrelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromField as PG

import qualified Prelude
import IHP.ViewPrelude (Html, TypeRep, hsx, typeOf, typeRep)
import Unsafe.Coerce

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

data JobsController
    = JobsAction
    | NewJobAction
    | NewFixSongJobAction
    | NewInitialScrapeJobAction
    | CreateNightlyScrapeJobAction
    | CreateFixSongJobAction
    | CreateInitialScrapeJobAction
    | ShowJobAction { jobId :: !(Id NightlyScrapeJob) }
    | ShowFixSongJobAction { fixSongJobId :: !(Id FixSongJob) }
    | ShowInitialScrapeJobAction { initialScrapeJobId :: !(Id InitialScrapeJob) }
    deriving (Eq, Show, Data)


