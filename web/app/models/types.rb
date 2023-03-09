class Types
  include Dry.Types()

  Date = Strict::Date | JSON::Date
  Symbol = Strict::Symbol | JSON::Symbol
  Time = Strict::Time | JSON::Time
  Decimal = Coercible::Decimal
end
