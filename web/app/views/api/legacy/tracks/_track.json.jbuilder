json.id track.id
json.title track.title.presence || track.file_name.presence || "Track #{track.track}"
json.file_name track.file_name.to_s
json.album track.album || track.file_name.to_s
json.track track.track
json.length track.length.to_s
json.recording_id track.recording_id
