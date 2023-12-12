// Configure your import map in config/importmap.rb. Read more: https://github.com/rails/importmap-rails
import "@hotwired/turbo-rails"

import { Gapless5 } from 'Gapless';
const queue = new Gapless5();

let listeners = {};

addEventListener('turbo:load', () => {
    const trackNodes = document.querySelectorAll('.browse__player__play_track')
    const playlistJson = JSON.parse(document.getElementById('playlist-json')?.dataset?.playlistJson || '{}');

    Array.from(trackNodes).forEach(trackNode => {
        if (listeners[trackNode]) {
            trackNode.removeEventListener('click', listeners[trackNode]);
        }

        const listener =  async (e) => {
            const { track } = e.currentTarget.dataset;
            const trackNumber = Number(track);
            const trackObject = playlistJson[trackNumber];

            if (queue.isPlaying()) {
                queue.stop()
                queue.removeAllTracks();
            }

            queue.addTrack(trackObject.media_url);
            queue.play();

            document.getElementById('player').innerHTML = `
                <h4>${trackObject.title}</h4>
                <a href="/recordings/${trackObject.recording_id}">Go to recording</a>
            `;
        };
        trackNode.addEventListener('click', listener);
        listeners[trackNode] = listener;
    })
});

