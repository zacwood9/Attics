Track.where("length not like '%:%'").find_in_batches do |batch|
  updates = batch.map do |track|
    { **track.attributes, length: Track.convert_to_minutes_seconds(track.length) }
  end

  Track.upsert_all updates, returning: false
end
