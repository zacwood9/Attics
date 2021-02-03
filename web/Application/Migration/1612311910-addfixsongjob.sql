-- Write your SQL migration code in here
CREATE TABLE fix_song_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    band_id UUID NOT NULL
);
ALTER TABLE fix_song_jobs ADD CONSTRAINT fix_song_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES bands (id) ON DELETE CASCADE;
