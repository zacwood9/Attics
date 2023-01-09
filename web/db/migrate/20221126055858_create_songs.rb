class CreateSongs < ActiveRecord::Migration[7.0]
  def change
    create_table :songs, id: :uuid do |t|
      t.string :file_name, null: false
      t.string :title
      t.integer :track, null: false
      t.string :creator
      t.string :length, null: false
      t.string :album
      t.references :recording, null: false, foreign_key: true, type: :uuid

      t.timestamps
    end
  end
end
