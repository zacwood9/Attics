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
            <img src="/icon.png" width="30" height="30" class="d-inline-block align-top mr-2" style="border-radius: 8px" alt="">
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
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app-v1.css"/>
    |]

scripts :: Html
scripts = do
    [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/ihp-auto-refresh.js"></script>
        <script src="/vendor/turbolinks.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/turbolinksMorphdom.js"></script>
        <script src="/vendor/turbolinksInstantClick.js"></script>
        <script src="/app-v1.js"></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Attics"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="https://attics.io"/>
    <meta property="og:description" content="Stream thousands of live concerts"/>
    <meta property="og:image" content="/icon.png" />

    <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
    <link rel="manifest" href="/site.webmanifest">
|]
