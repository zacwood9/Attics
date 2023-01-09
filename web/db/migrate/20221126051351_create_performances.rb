class CreatePerformances < ActiveRecord::Migration[7.0]
  def change
    create_table :performances, id: :uuid do |t|
      t.date :date, null: false
      t.string :venue
      t.string :city
      t.string :state
      t.references :band, null: false, foreign_key: true, type: :uuid

      t.timestamps
    end
  end
end
