json.band { json.partial! @band }

json.top_performances do
  @top_performances.each do |year, performances|
    json.set! year do
      json.partial! "api/legacy/performances/performance", collection: performances, as: :performance
    end
  end
end

json.on_this_day do
  json.partial! "api/legacy/performances/performance", collection: @on_this_day_performances, as: :performance
end
