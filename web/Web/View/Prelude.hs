module Web.View.Prelude
  ( module IHP.ViewPrelude,
    module Web.View.Layout,
    module Generated.Types,
    module Web.Types,
    module Application.Helper.View,
    comingSoon,
  )
where

import Application.Helper.View
import Generated.Types
import IHP.ViewPrelude
import Web.Routes ()
import Web.Types
import Web.View.Layout

comingSoon :: Html
comingSoon =
  [hsx|
         <div style="background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
              <div style="max-width: 800px; margin-left: auto; margin-right: auto">
                  <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">
                      Attics Web
                  </h1>

                  <h2 style="margin-top: 0; margin-bottom: 0rem; font-weight: 900; font-size: 3rem">
                      Coming soon.
                  </h2>

                  <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                     For now, download Attics on your iOS device!
                  </p>

                  <a href="https://apps.apple.com/us/app/attics/id1434981632"><img src="/app-store.svg" height="50"></a>
              </div>
         </div>

         <div style="max-width: 800px; margin-left: auto; margin-right: auto; margin-top: 4rem">
              <img src="/ihp-welcome-icon.svg" alt="/ihp-welcome-icon">
         </div>
|]