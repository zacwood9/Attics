class PerformanceBlueprint < BaseBlueprint
  identifier :id
  fields :date, :band_id

  field_with_default :city, "Unknown"
  field_with_default :state, "Unknown"
  field_with_default :venue, "Unknown Venue"

  view :with_metadata do
    field_with_default :avg_rating, 0
    field_with_default :num_recordings, 0
    field_with_default :num_reviews, 0
  end
end
