class CreateRecordings < ActiveRecord::Migration[7.0]
  def change
    create_table :recordings, id: :uuid do |t|
      t.string :transferer
      t.string :source
      t.string :lineage
      t.integer :archive_downloads, null: false, default: 0
      t.float :avg_rating, null: false, default: 0
      t.integer :num_reviews, null: false, default: 0
      t.integer :attics_downloads, null: false, default: 0
      t.string :identifier, null: false
      t.references :performance, null: false, foreign_key: true, type: :uuid

      t.timestamps
    end
  end
end
