module Health.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data HealthApplication = HealthApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)
