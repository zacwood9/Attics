module Health.View.Static.Welcome where
import Health.View.Prelude
import Data.Aeson

data WelcomeView = WelcomeView {
    canQueryDatabase :: Bool,
    queryTime :: Text
}

instance View WelcomeView where
    html view@WelcomeView { .. } = let
        in [hsx|
         <div style="background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
              <div style="max-width: 800px; margin-left: auto; margin-right: auto">
                  <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">
                      Attics: Application Health
                  </h1>

                  <h2 style="margin-top: 0; margin-bottom: 0rem; font-weight: 900; font-size: 3rem">
                      Status: {getStatus view}
                  </h2>

                  <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                     Database connection: {databaseStatus view}
                  </p>
                  <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                     Request time: {queryTime}
                  </p>
              </div>
         </div>
|]

    json = toJSON

getStatus :: WelcomeView -> Text
getStatus WelcomeView { .. } =
    if canQueryDatabase
        then "Good"
        else "Having some problems..."

databaseStatus :: WelcomeView -> Text
databaseStatus view = if get #canQueryDatabase view then "Up" else "Down"

instance ToJSON WelcomeView where
    toJSON view = object [
        "status" .= getStatus view,
        "database_status" .= databaseStatus view,
        "query_time" .= get #queryTime view
        ]

