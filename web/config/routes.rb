Rails.application.routes.draw do
  root to: "welcome#index"
  get "/.well-known/apple-app-site-association", to: "apple_app_site_association#index"

  namespace :admin do
    root to: 'bands#index'

    resources :bands
  end

  scope :api do
    scope :legacy do
      get "/bands", to: "bands#index", as: :bands
      get "/bands/:collection/performances/top", to: "performances#top", as: :band_top_performances
      get "/bands/:collection/performances", to: "performances#index", as: :band_performances
      get "/bands/:collection/performances/:date/recordings", to: "recordings#index", as: :performance_recordings
      get "/recordings/:identifier/tracks", to: "tracks#index", as: :recording_tracks, identifier: /.*/
    end
  end
end
