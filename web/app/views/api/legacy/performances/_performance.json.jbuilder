json.id performance.id
json.date performance.date
json.venue performance.venue.to_s
json.city performance.city.to_s
json.state performance.state.to_s
json.num_reviews performance.num_reviews || 0
json.num_recordings performance.num_recordings || 0
json.avg_rating performance.avg_rating || 0
json.band_id performance.band_id
