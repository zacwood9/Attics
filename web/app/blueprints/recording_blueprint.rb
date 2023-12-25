class RecordingBlueprint < BaseBlueprint
  identifier :id
  fields :identifier, :performance_id

  field_with_default :transferer, "Unknown transferer"
  field_with_default :source, "Unknown source"
  field_with_default :avg_rating, 0
  field_with_default :archive_downloads, 0
  field_with_default :num_reviews, 0
  field_with_default :lineage, "Unknown lineage"
end
