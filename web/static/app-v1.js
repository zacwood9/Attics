window.addEventListener('turbolinks:load', () => {
    const url = new URL(window.location)
    const params = new URLSearchParams(url.search)

    const selectedTrack = params.get('selectedTrack')
    if (selectedTrack != null) {
        const songId = `song-${selectedTrack}`
        const songEl = document.getElementById(songId);
        if (songEl != null) {
            const audioPlayer = document.getElementById('audio');
            const songUrl = songEl.dataset.url;
            audioPlayer.src = songUrl;
            audioPlayer.play();

            audioPlayer.addEventListener("ended", function() {
                url.searchParams.set('selectedTrack', `${Number(selectedTrack) + 1}`);
                window.location = url.href;
            });
        }
    }
});
