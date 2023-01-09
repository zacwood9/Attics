class CreateBands < ActiveRecord::Migration[7.0]
  def change
    create_table :bands, id: :uuid do |t|
      t.string :name, null: false
      t.string :collection, null: false
      t.string :logo_url
      t.string :url

      t.timestamps
    end
  end
end
