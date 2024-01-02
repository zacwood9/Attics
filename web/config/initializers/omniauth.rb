Rails.application.config.middleware.use OmniAuth::Builder do
  provider :developer unless Rails.env.production? # You should replace it with your provider

  provider :apple,
           Rails.application.credentials.dig(:apple, :client_id),
           "",
           {
             scope: "email name",
             team_id: Rails.application.credentials.dig(:apple, :team_id),
             key_id: Rails.application.credentials.dig(:apple, :key_id),
             pem: Rails.application.credentials.dig(:apple, :pem)
           }
end
