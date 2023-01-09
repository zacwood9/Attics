--
-- PostgreSQL database dump
--

-- Dumped from database version 11.14
-- Dumped by pg_dump version 14.5 (Homebrew)

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

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: job_status; Type: TYPE; Schema: public; Owner: attics
--

CREATE TYPE public.job_status AS ENUM (
    'job_status_not_started',
    'job_status_running',
    'job_status_failed',
    'job_status_succeeded',
    'job_status_retry'
);


ALTER TYPE public.job_status OWNER TO attics;

--
-- Name: notify_did_change_bands(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_did_change_bands() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                BEGIN
                    PERFORM pg_notify('did_change_bands', '');
                    RETURN new;
                END;
            $$;


ALTER FUNCTION public.notify_did_change_bands() OWNER TO attics;

--
-- Name: notify_did_change_fix_song_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_did_change_fix_song_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
    PERFORM pg_notify('did_change_fix_song_jobs', '');
    RETURN new;END;
$$;


ALTER FUNCTION public.notify_did_change_fix_song_jobs() OWNER TO attics;

--
-- Name: notify_did_change_initial_scrape_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_did_change_initial_scrape_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                BEGIN
                    PERFORM pg_notify('did_change_initial_scrape_jobs', '');
                    RETURN new;
                END;
            $$;


ALTER FUNCTION public.notify_did_change_initial_scrape_jobs() OWNER TO attics;

--
-- Name: notify_did_change_nightly_scrape_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_did_change_nightly_scrape_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                BEGIN
                    PERFORM pg_notify('did_change_nightly_scrape_jobs', '');
                    RETURN new;
                END;
            $$;


ALTER FUNCTION public.notify_did_change_nightly_scrape_jobs() OWNER TO attics;

--
-- Name: notify_job_queued_fix_song_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_job_queued_fix_song_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
    PERFORM pg_notify('job_available_fix_song_jobs', '');
    RETURN new;END;
$$;


ALTER FUNCTION public.notify_job_queued_fix_song_jobs() OWNER TO attics;

--
-- Name: notify_job_queued_initial_scrape_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_job_queued_initial_scrape_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
    PERFORM pg_notify('job_available_initial_scrape_jobs', '');
    RETURN new;END;
$$;


ALTER FUNCTION public.notify_job_queued_initial_scrape_jobs() OWNER TO attics;

--
-- Name: notify_job_queued_nightly_scrape_jobs(); Type: FUNCTION; Schema: public; Owner: attics
--

CREATE FUNCTION public.notify_job_queued_nightly_scrape_jobs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
    PERFORM pg_notify('job_available_nightly_scrape_jobs', '');
    RETURN new;END;
$$;


ALTER FUNCTION public.notify_job_queued_nightly_scrape_jobs() OWNER TO attics;

SET default_tablespace = '';

--
-- Name: bands; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.bands (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    collection text NOT NULL,
    name text NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    url text DEFAULT ''::text NOT NULL,
    logo_url text
);


ALTER TABLE public.bands OWNER TO attics;

--
-- Name: fix_song_jobs; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.fix_song_jobs (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    status public.job_status DEFAULT 'job_status_not_started'::public.job_status NOT NULL,
    last_error text,
    attempts_count integer DEFAULT 0 NOT NULL,
    locked_at timestamp with time zone,
    locked_by uuid,
    band_id uuid NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.fix_song_jobs OWNER TO attics;

--
-- Name: initial_scrape_jobs; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.initial_scrape_jobs (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    status public.job_status DEFAULT 'job_status_not_started'::public.job_status NOT NULL,
    last_error text,
    attempts_count integer DEFAULT 0 NOT NULL,
    locked_at timestamp with time zone,
    locked_by uuid,
    band_id uuid NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.initial_scrape_jobs OWNER TO attics;

--
-- Name: nightly_scrape_jobs; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.nightly_scrape_jobs (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    status public.job_status DEFAULT 'job_status_not_started'::public.job_status NOT NULL,
    last_error text,
    attempts_count integer DEFAULT 0 NOT NULL,
    locked_at timestamp with time zone,
    locked_by uuid,
    band_id uuid NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.nightly_scrape_jobs OWNER TO attics;

--
-- Name: performances; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.performances (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    band_id uuid NOT NULL,
    date text NOT NULL,
    venue text NOT NULL,
    city text NOT NULL,
    state text NOT NULL
);


ALTER TABLE public.performances OWNER TO attics;

--
-- Name: recordings; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.recordings (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    performance_id uuid NOT NULL,
    transferer text NOT NULL,
    source text NOT NULL,
    lineage text NOT NULL,
    archive_downloads integer DEFAULT 0 NOT NULL,
    avg_rating double precision DEFAULT 0 NOT NULL,
    num_reviews integer DEFAULT 0 NOT NULL,
    attics_downloads integer DEFAULT 0 NOT NULL,
    identifier text NOT NULL
);


ALTER TABLE public.recordings OWNER TO attics;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.schema_migrations (
    revision bigint NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO attics;

--
-- Name: songs; Type: TABLE; Schema: public; Owner: attics
--

CREATE TABLE public.songs (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    file_name text NOT NULL,
    title text NOT NULL,
    track integer NOT NULL,
    creator text NOT NULL,
    length text NOT NULL,
    album text NOT NULL,
    recording_id uuid NOT NULL
);


ALTER TABLE public.songs OWNER TO attics;

--
-- Name: bands bands_collection_key; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.bands
    ADD CONSTRAINT bands_collection_key UNIQUE (collection);


--
-- Name: bands bands_name_key; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.bands
    ADD CONSTRAINT bands_name_key UNIQUE (name);


--
-- Name: bands bands_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.bands
    ADD CONSTRAINT bands_pkey PRIMARY KEY (id);


--
-- Name: fix_song_jobs fix_song_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.fix_song_jobs
    ADD CONSTRAINT fix_song_jobs_pkey PRIMARY KEY (id);


--
-- Name: initial_scrape_jobs initial_scrape_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.initial_scrape_jobs
    ADD CONSTRAINT initial_scrape_jobs_pkey PRIMARY KEY (id);


--
-- Name: nightly_scrape_jobs nightly_scrape_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.nightly_scrape_jobs
    ADD CONSTRAINT nightly_scrape_jobs_pkey PRIMARY KEY (id);


--
-- Name: performances performances_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.performances
    ADD CONSTRAINT performances_pkey PRIMARY KEY (id);


--
-- Name: recordings recordings_identifier_key; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.recordings
    ADD CONSTRAINT recordings_identifier_key UNIQUE (identifier);


--
-- Name: recordings recordings_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.recordings
    ADD CONSTRAINT recordings_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_revision_key; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_revision_key UNIQUE (revision);


--
-- Name: songs songs_pkey; Type: CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.songs
    ADD CONSTRAINT songs_pkey PRIMARY KEY (id);


--
-- Name: bands did_delete_bands; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_delete_bands AFTER DELETE ON public.bands FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_bands();


--
-- Name: fix_song_jobs did_delete_fix_song_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_delete_fix_song_jobs AFTER DELETE ON public.fix_song_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_fix_song_jobs();


--
-- Name: initial_scrape_jobs did_delete_initial_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_delete_initial_scrape_jobs AFTER DELETE ON public.initial_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_initial_scrape_jobs();


--
-- Name: nightly_scrape_jobs did_delete_nightly_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_delete_nightly_scrape_jobs AFTER DELETE ON public.nightly_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_nightly_scrape_jobs();


--
-- Name: bands did_insert_bands; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_bands AFTER INSERT ON public.bands FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_bands();


--
-- Name: fix_song_jobs did_insert_fix_song_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_fix_song_jobs AFTER INSERT ON public.fix_song_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_fix_song_jobs();


--
-- Name: initial_scrape_jobs did_insert_initial_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_initial_scrape_jobs AFTER INSERT ON public.initial_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_initial_scrape_jobs();


--
-- Name: fix_song_jobs did_insert_job_fix_song_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_job_fix_song_jobs AFTER INSERT ON public.fix_song_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_fix_song_jobs();


--
-- Name: initial_scrape_jobs did_insert_job_initial_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_job_initial_scrape_jobs AFTER INSERT ON public.initial_scrape_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_initial_scrape_jobs();


--
-- Name: nightly_scrape_jobs did_insert_job_nightly_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_job_nightly_scrape_jobs AFTER INSERT ON public.nightly_scrape_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_nightly_scrape_jobs();


--
-- Name: nightly_scrape_jobs did_insert_nightly_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_insert_nightly_scrape_jobs AFTER INSERT ON public.nightly_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_nightly_scrape_jobs();


--
-- Name: bands did_update_bands; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_bands AFTER UPDATE ON public.bands FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_bands();


--
-- Name: fix_song_jobs did_update_fix_song_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_fix_song_jobs AFTER UPDATE ON public.fix_song_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_fix_song_jobs();


--
-- Name: initial_scrape_jobs did_update_initial_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_initial_scrape_jobs AFTER UPDATE ON public.initial_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_initial_scrape_jobs();


--
-- Name: fix_song_jobs did_update_job_fix_song_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_job_fix_song_jobs AFTER UPDATE ON public.fix_song_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_fix_song_jobs();


--
-- Name: initial_scrape_jobs did_update_job_initial_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_job_initial_scrape_jobs AFTER UPDATE ON public.initial_scrape_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_initial_scrape_jobs();


--
-- Name: nightly_scrape_jobs did_update_job_nightly_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_job_nightly_scrape_jobs AFTER UPDATE ON public.nightly_scrape_jobs FOR EACH ROW WHEN (((new.status = 'job_status_not_started'::public.job_status) OR (new.status = 'job_status_retry'::public.job_status))) EXECUTE PROCEDURE public.notify_job_queued_nightly_scrape_jobs();


--
-- Name: nightly_scrape_jobs did_update_nightly_scrape_jobs; Type: TRIGGER; Schema: public; Owner: attics
--

CREATE TRIGGER did_update_nightly_scrape_jobs AFTER UPDATE ON public.nightly_scrape_jobs FOR EACH STATEMENT EXECUTE PROCEDURE public.notify_did_change_nightly_scrape_jobs();


--
-- Name: fix_song_jobs fix_song_jobs_ref_band_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.fix_song_jobs
    ADD CONSTRAINT fix_song_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES public.bands(id) ON DELETE CASCADE;


--
-- Name: initial_scrape_jobs initial_scrape_jobs_ref_band_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.initial_scrape_jobs
    ADD CONSTRAINT initial_scrape_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES public.bands(id) ON DELETE CASCADE;


--
-- Name: nightly_scrape_jobs nightly_scrape_jobs_ref_band_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.nightly_scrape_jobs
    ADD CONSTRAINT nightly_scrape_jobs_ref_band_id FOREIGN KEY (band_id) REFERENCES public.bands(id) ON DELETE CASCADE;


--
-- Name: performances performances_ref_band_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.performances
    ADD CONSTRAINT performances_ref_band_id FOREIGN KEY (band_id) REFERENCES public.bands(id) ON DELETE CASCADE;


--
-- Name: recordings recordings_ref_performance_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.recordings
    ADD CONSTRAINT recordings_ref_performance_id FOREIGN KEY (performance_id) REFERENCES public.performances(id) ON DELETE CASCADE;


--
-- Name: songs songs_ref_recording_id; Type: FK CONSTRAINT; Schema: public; Owner: attics
--

ALTER TABLE ONLY public.songs
    ADD CONSTRAINT songs_ref_recording_id FOREIGN KEY (recording_id) REFERENCES public.recordings(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

