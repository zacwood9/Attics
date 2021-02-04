module Health.Controller.Static where
import Health.Controller.Prelude
import Health.View.Static.Welcome
import Control.Exception (try)
import Data.Either (isRight)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, nominalDiffTimeToSeconds)

instance Controller StaticController where
    action WelcomeAction = do
        startTime <- getCurrentTime
        queryResult :: Either SomeException Band <- try (query @Band |> fetchOne)
        let canQueryDatabase = isRight queryResult
        endTime <- getCurrentTime

        let queryTime =  show ((*) 1000 $ nominalDiffTimeToSeconds $ endTime `diffUTCTime` startTime) <> "ms"

        render WelcomeView { .. }
