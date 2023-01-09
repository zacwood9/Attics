json.band do
  json.extract! @band,
                :id,
                :collection,
                :name,
                :logo_url
end

json.top_performances do
  @top_performances.each do |year, performances|
    json.set! year do
      json.array! performances do |performance|
        json.extract! performance, :id, :date, :venue, :city, :state, :num_reviews, :num_recordings, :avg_rating, :band_id
      end
    end
  end
end
