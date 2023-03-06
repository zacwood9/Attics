require 'pg'

PG.connect(Rails.application.credentials[:legacy_database_url]) do |conn|
  pp conn.exec("select count(*) from bands")
end
