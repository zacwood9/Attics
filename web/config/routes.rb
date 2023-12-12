Rails.application.routes.draw do
  get  "sign_in", to: "sessions#new"
  post "sign_in", to: "sessions#create"
  get  "sign_up", to: "registrations#new"
  post "sign_up", to: "registrations#create"
  resources :sessions, only: [:index, :show, :destroy]
  resource  :password, only: [:edit, :update]
  resource :account, only: [:show]
  namespace :identity do
    resource :email,              only: [:edit, :update]
    resource :email_verification, only: [:show, :create]
    resource :password_reset,     only: [:new, :edit, :create, :update]
  end
  namespace :authentications do
    resources :events, only: :index
  end
  get  "/auth/failure",            to: "sessions/omniauth#failure"
  get  "/auth/:provider/callback", to: "sessions/omniauth#create"
  post "/auth/:provider/callback", to: "sessions/omniauth#create"

  root "home#index"

  resources :bands, only: [:show] do
    resources :years, only: [:show], param: :year
  end

  resources :performances, only: [:show]
  resources :recordings, only: [:show]

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
