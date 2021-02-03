-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE bands (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    collection TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL UNIQUE,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    url TEXT DEFAULT '' NOT NULL,
    logo_url TEXT DEFAULT NULL
);
CREATE TABLE performances (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    band_id UUID NOT NULL,
    date TEXT NOT NULL,
    venue TEXT NOT NULL,
    city TEXT NOT NULL,
    state TEXT NOT NULL
);
CREATE TABLE recordings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    performance_id UUID NOT NULL,
    transferer TEXT NOT NULL,
    source TEXT NOT NULL,
    lineage TEXT NOT NULL,
    archive_downloads INT DEFAULT 0 NOT NULL,
    avg_rating DOUBLE PRECISION DEFAULT 0 NOT NULL,
    num_reviews INT DEFAULT 0 NOT NULL,
    attics_downloads INT DEFAULT 0 NOT NULL,
    identifier TEXT NOT NULL UNIQUE
);
CREATE TABLE songs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    file_name TEXT NOT NULL,
    title TEXT NOT NULL,
    track INT NOT NULL,
    creator TEXT NOT NULL,
    length TEXT NOT NULL,
    album TEXT NOT NULL,
    recording_id UUID NOT NULL
);
CREATE TABLE nightly_scrape_jobs (
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
ALTER TABLE fix_song_jobs ADD CONSTRAINT fix_song_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES bands (id) ON DELETE NO ACTION;
ALTER TABLE nightly_scrape_jobs ADD CONSTRAINT nightly_scrape_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES bands (id) ON DELETE CASCADE;
ALTER TABLE performances ADD CONSTRAINT performances_ref_band_id FOREIGN KEY (band_id) REFERENCES bands (id) ON DELETE CASCADE;
ALTER TABLE recordings ADD CONSTRAINT recordings_ref_performance_id FOREIGN KEY (performance_id) REFERENCES performances (id) ON DELETE CASCADE;
ALTER TABLE songs ADD CONSTRAINT songs_ref_recording_id FOREIGN KEY (recording_id) REFERENCES recordings (id) ON DELETE CASCADE;
