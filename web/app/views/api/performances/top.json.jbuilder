json.band { json.partial! @band }

json.top_performances do
  @top_performances.each do |year, performances|
    json.set! year do
      json.array! performances, partial: 'api/performances/performance', as: :performance
    end
  end
end

json.on_this_day do
  json.array! @on_this_day_performances, partial: 'api/performances/performance', as: :performance
end
