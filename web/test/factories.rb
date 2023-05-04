FactoryBot.define do
  factory :band do
    name { "Grateful Dead" }
    collection { name.split(" ").join }
    logo_url { "https://archive.org/services/img/#{collection}" }
  end

  factory :performance do
    band
    date { "1977-05-08" }
    city { "Ithica" }
    state { "NY" }
    venue { "Madison Square Garden" }
  end

  factory :recording do
    performance
    identifier { "gd1977-05-08.sbd.miller.92094.sbeok.flac16" }
    avg_rating { 4.5 }
    num_reviews { 25 }
  end
end

def performance_with_recordings(num_recordings: 3)
  FactoryBot.create(:performance) do |performance|
    FactoryBot.create_list(:recording, num_recordings, performance: performance)
  end
end
