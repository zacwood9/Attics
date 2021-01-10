module Admin.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

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

data OtherController = OtherAction
    deriving (Eq, Show, Data)
