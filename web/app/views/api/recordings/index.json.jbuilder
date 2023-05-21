json.band { json.partial! @band }
json.performance { json.partial! @performance }
json.recordings { json.partial! 'api/recordings/recording', collection: @recordings, as: :recording }
