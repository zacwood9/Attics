module Web.View.Attics.Player where
import Web.View.Prelude
import Debug.Trace

data PlayerView = PlayerView {
    band :: Band,
    performance :: Performance,
    selectedRecording :: Recording,
    recordings :: [Recording],
    songs :: [Song]
}

instance View PlayerView where
    html v@PlayerView { .. } =
        let
            makeLink recording = pathTo (PlayerAction (get #collection band) (get #date performance) (Just $ get #identifier recording) Nothing)
            songLink track = pathTo (PlayerAction (get #collection band) (get #date performance) (Just $ get #identifier selectedRecording) (Just track))
            archiveLink song = "https://archive.org/download/" <> get #identifier selectedRecording <> "/" <> get #fileName song
        in [hsx|
        <div class="container-fluid">
            <div class="row mt-4">
                <div class="col-xl-9 col-md-7 col-12">
                    {player v archiveLink songLink}
                </div>
                <div class="col-xl-3 col-md-5 col mt-4">
                    {recordingsMenu recordings selectedRecording makeLink}
                </div>
            </div>
        </div>
    |]

player PlayerView { .. } archiveLink songLink = [hsx|
    <div class="text-center">
        <h2>{get #name band}</h2>
        <h4>Live at {get #venue performance}</h4>
        <h4>{get #date performance}</h4>
    </div>
    {renderSongs songs archiveLink songLink}
|]

renderSongs songs archiveLink songLink = [hsx|
    <div class="d-flex flex-column mx-auto" style="max-width: 50em">
        {forEach songs (renderSong archiveLink songLink)}
    </div>
|]

renderSong archiveLink songLink song =
    let
        id :: Text = "song-" <> show (get #track song)
        url :: Text = archiveLink song
    in [hsx|
    <a id={id}
       href={songLink (get #track song)}
       data-url={url}
       class="song-card"
       data-turbolinks-preload="false"
       data-turbolinks-action="replace">
        <div class="d-flex justify-content-between song">
            <div class="flex-column">
                <span class="text-secondary mr-2" >{get #track song}.</span>
                <span>{get #title song}</span>
            </div>
            <span class="text-secondary">{get #length song}</span>
        </div>
        <hr>
    </a>
|]

recordingsMenu recordings selected makeLink = [hsx|
    <div class="d-flex flex-column">
        <h4>Available Recordings</h4>
        {forEachWithIndex recordings (renderRecording selected makeLink)}
    </div>
|]

renderRecording selected makeLink (i, recording) =
    let
        rating' = (get #avgRating recording) / 5 * 100
        isSelected = selected == recording
        cardClass :: Text = if isSelected then "card recording-card recording-card-selected" else "card recording-card"
    in [hsx|
    <a href={makeLink recording} data-turbolinks-action="replace">
        <div class={cardClass}>
            <div class="card-body">
                <h5 class="card-title d-flex justify-content-between">
                    {get #transferer recording}
                    {rating rating'}
                </h5>
                <h6 class="card-subtitle d-flex justify-content-between">
                    <span class="text-muted">{show (get #archiveDownloads recording) <> " downloads"}</span>
                    <span>{recordingType recording}</span>
                </h6>
            </div>
        </div>
    </a>
|]
