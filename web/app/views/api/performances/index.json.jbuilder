json.band { json.partial! @band }
json.performances { json.partial! 'api/performances/performance', collection: @performances, as: :performance }
