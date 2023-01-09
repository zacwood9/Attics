class RenameSongsToTracks < ActiveRecord::Migration[7.0]
  def change
    rename_table :songs, :tracks
  end
end
