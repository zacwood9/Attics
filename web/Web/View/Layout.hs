module Web.View.Layout (defaultLayout, homeLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes


defaultLayout :: (?context :: ControllerContext) => Html -> Html
defaultLayout inner =
    let
        contextTitle = maybeFromFrozenContext @PageTitle
        title = case contextTitle of
            Just (PageTitle title) -> "Attics: " <> title
            Nothing -> "Attics"
    in H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{title}</title>
</head>
<body>
    <nav class="navbar navbar-dark bg-attics-blue">
        <a class="navbar-brand" href={BandsAction}>
            <img src={assetPath "/icon.png"} width="30" height="30" class="d-inline-block align-top mr-2" style="border-radius: 8px" alt="">
            Attics
        </a>
    </nav>
    {renderFlashMessages}
    {inner}
</body>
|]



homeLayout :: Html -> Html
homeLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>Attics</title>
</head>
<body>
    {inner}
</body>
|]

stylesheets :: Html
stylesheets = do
    [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = do
    [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Attics"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="https://attics.io"/>
    <meta property="og:description" content="Stream thousands of live concerts"/>
    <meta property="og:image" content={assetPath "/icon.png"} />

    <link rel="apple-touch-icon" sizes="180x180" href={assetPath "/apple-touch-icon.png"}>
    <link rel="icon" type="image/png" sizes="32x32" href={assetPath "/favicon-32x32.png"}>
    <link rel="icon" type="image/png" sizes="16x16" href={assetPath "/favicon-16x16.png"}>
    <link rel="manifest" href={assetPath "/site.webmanifest"}>
|]
