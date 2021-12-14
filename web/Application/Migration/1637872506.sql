-- Write your SQL migration code in here
alter table fix_song_jobs add column run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;
update fix_song_jobs set run_at = created_at;
alter table nightly_scrape_jobs add column run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;
update nightly_scrape_jobs set run_at = created_at;
alter table initial_scrape_jobs add column run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL;
update initial_scrape_jobs set run_at = created_at;