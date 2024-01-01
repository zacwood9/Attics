json.band { json.partial! @band }
json.performances { json.partial! "api/legacy/performances/performance", collection: @performances, as: :performance }
