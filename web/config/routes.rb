Rails.application.routes.draw do
  root to: "welcome#index"

  get "/.well-known/apple-app-site-association",
      to: "apple_app_site_association#index"

  namespace :admin do
    root to: "bands#index"

    resources :bands
  end

  scope :api do
    scope :legacy do
      get "/bands", to: "api/bands#index", as: :api_bands
      get "/bands/:collection/performances/top",
          to: "api/performances#top",
          as: :api_band_top_performances,
          collection: /.*/
      get "/bands/:collection/performances/on_this_day",
          to: "api/performances#top",
          as: :api_band_on_this_day_performances,
          collection: /.*/
      get "/bands/:collection/performances",
          to: "api/performances#index",
          as: :api_band_performances,
          collection: /.*/
      get "/bands/:collection/performances/:date/recordings",
          to: "api/recordings#index",
          as: :api_performance_recordings,
          collection: /.*/
      get "/recordings/:identifier/tracks",
          to: "api/tracks#index",
          as: :api_recording_tracks,
          identifier: /.*/
    end
  end
end
