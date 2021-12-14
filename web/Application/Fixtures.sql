

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.bands DISABLE TRIGGER ALL;

INSERT INTO public.bands (id, collection, name, updated_at, url, logo_url) VALUES ('7201e6f2-d078-47aa-bae4-e1c4e4d70c38', 'BillyStrings', 'Billy Strings', '2021-11-25 15:44:28.527974-05', '', '');


ALTER TABLE public.bands ENABLE TRIGGER ALL;


ALTER TABLE public.fix_song_jobs DISABLE TRIGGER ALL;



ALTER TABLE public.fix_song_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.initial_scrape_jobs DISABLE TRIGGER ALL;

INSERT INTO public.initial_scrape_jobs (id, created_at, updated_at, status, last_error, attempts_count, locked_at, locked_by, band_id, run_at) VALUES ('699317de-af3e-45f1-9f35-59ad38f82482', '2021-11-25 15:44:33.538187-05', '2021-11-25 15:44:33.538187-05', 'job_status_running', NULL, 1, '2021-11-25 15:44:33.550333-05', '1c4b42b2-5335-4674-a060-8c7e3caee315', '7201e6f2-d078-47aa-bae4-e1c4e4d70c38', '2021-11-25 15:44:33.538187-05');


ALTER TABLE public.initial_scrape_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.my_test_jobs DISABLE TRIGGER ALL;



ALTER TABLE public.my_test_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.nightly_scrape_jobs DISABLE TRIGGER ALL;



ALTER TABLE public.nightly_scrape_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.performances DISABLE TRIGGER ALL;



ALTER TABLE public.performances ENABLE TRIGGER ALL;


ALTER TABLE public.recordings DISABLE TRIGGER ALL;



ALTER TABLE public.recordings ENABLE TRIGGER ALL;


ALTER TABLE public.schema_migrations DISABLE TRIGGER ALL;

INSERT INTO public.schema_migrations (revision) VALUES (1637872506);


ALTER TABLE public.schema_migrations ENABLE TRIGGER ALL;


ALTER TABLE public.songs DISABLE TRIGGER ALL;



ALTER TABLE public.songs ENABLE TRIGGER ALL;


