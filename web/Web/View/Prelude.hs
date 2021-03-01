module Web.View.Prelude
  ( module IHP.ViewPrelude,
    module Web.View.Layout,
    module Generated.Types,
    module Web.Types,
    module Application.Helper.View,
    HomeView(..),
    rating,
    recordingType
  )
where

import Application.Helper.View
import Generated.Types
import IHP.ViewPrelude
import Web.Routes ()
import Web.Types
import Web.View.Layout
import IHP.ControllerPrelude (render, fetchCount)
import qualified Text.Show

rating value =
    let frontStyle = "width: " <> show value <> "%" in [hsx|
    <div class="star-rating">
        <div class="back-stars">
            {emptyStar}
            {emptyStar}
            {emptyStar}
            {emptyStar}
            {emptyStar}

            <div class="front-stars" style={frontStyle}>
                {fullStar}
                {fullStar}
                {fullStar}
                {fullStar}
                {fullStar}
            </div>
        </div>
    </div>
|]

emptyStar = [hsx|<span>☆</span>|]
fullStar = [hsx|<span>★</span>|]

-- var type: RecordingType {
--         let s = source.lowercased()
--         for type in ["mtx", "matrix"] {
--             if s.contains(type) { return .matrix }
--         }
--         for type in ["aud"] {
--             if s.contains(type) { return .aud }
--         }
--         for type in ["sbd", "soundboard"] {
--             if s.contains(type) { return .sbd }
--         }
--         return .unknown
--     }
--

data RecordingType = Matrix | Audience | Soundboard | Unknown

recordingType' :: RecordingType -> Text
recordingType' Matrix = "MTX"
recordingType' Audience = "AUD"
recordingType' Soundboard = "SBD"
recordingType' _ = ""

recordingType :: Recording -> Text
recordingType Recording { source } = recordingType' Matrix


data HomeView = HomeView {
  bands :: Int,
  shows :: Int
}

instance View HomeView where
  html HomeView { .. } = let
    headerLine = show bands <> " bands, " <> show shows <> " concerts, on the go."
    in [hsx|

<div class="home-page">
<div class="jumbotron jumbotron-fluid">

  <div class="container-md">

  <header class="row" style="margin-bottom: 4em">
    <div class="col">
      <div class="d-flex align-items-center">
        <img class="mr-4" src="/icon.png" height="128" style="border-radius: 16px">
        <div class="d-flex flex-column">
          <h1 class="text-white display-3 ">Attics</h1>
          <span class="lead">{headerLine}</span>
        </div>
      </div>
    </div>

    <div class="col-12 col-lg">
      <div class="download-button" style="color: white">
          <a href="https://apps.apple.com/us/app/attics/id1434981632"><img src="/app-store.svg" height="50"></a>
      </div>
    </div>
  </header>

  </div>

  <div class="container-md">
    <div class="row">

      <div class="col-12 col-lg">
        <div class="d-flex flex-column align-items-center">
          <img src="/iphone-1.png" width="300px">
          <h4 class="text-center">Find the best shows.</h4>
        </div>
      </div>

      <div class="col-12 col-lg">
        <div class="d-flex flex-column align-items-center">
          <img src="/iphone-myshows.png" width="300px">
          <h4 class="text-center">Save and download your favorites.</h4>
        </div>
      </div>

      <div class="col-12 col-lg">
        <div class="d-flex flex-column align-items-center">
          <img src="/iphone-player.png" width="300px">
          <h4 class="text-center">Gapless playback makes it feel like you're there.</h4>
        </div>
      </div>

    </div>
  </div>

  <div class="container-md" style="margin-top: 12em;">
    <div class="d-flex justify-content-center">
      <a href="mailto:zac.wood@hey.com"><button class="btn btn-primary" type="submit">Send feedback</button></a>
    </div>
  </div>

  <footer class="footer">
    <div class="container">
      <p class="text-muted">
        Attics is created by Zachary Wood. Powered by <a href="https://ihp.digitallyinduced.com">IHP</a>. View the source code on <a href="https://github.com/zacwood9/Attics">GitHub</a>.
      </p>
      <p class="text-muted">
        All music on Attics is streamed with permission from the <a href="https://archive.org/details/etree">Live Music Archive</a>.
      </p>
      <p class="text-muted">
        <span>Photo by <a href="https://unsplash.com/@spencerdavis?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Spencer Davis</a> on <a href="https://unsplash.com/s/photos/concert?utm_source=unsplash&amp;utm_medium=referral&amp;utm_content=creditCopyText">Unsplash</a></span>
      </p>
    </div>
  </footer>

</div>
</div>

|]


