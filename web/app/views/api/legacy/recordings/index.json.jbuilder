json.band { json.partial! @band }
json.performance { json.partial! @performance }
json.recordings { json.partial! 'api/legacy/recordings/recording', collection: @recordings, as: :recording }
