--------------------------------------------------------------------------
-- Initialize maintenance support for migrations tables.
--
--   see '001.sql' for an example usage
--------------------------------------------------------------------------

BEGIN;

DO $$
DECLARE
  initial_version text := '0';
BEGIN
  -- Check if the maintenance schema exists.
  -- Only proceed if it doesn't exist.
  IF EXISTS (SELECT table_name
               FROM information_schema.tables
               WHERE (table_schema = 'maintenance')
            )
  THEN
    RAISE EXCEPTION 'SKIPPING: migration initialization for %', initial_version;
  END IF;

  -- All of our schemas and tables are created as the nixtodo user.
  SET LOCAL ROLE nixtodo;


  --------------------------------------------------------------------------
  -- Initialize maintenance support
  --------------------------------------------------------------------------

  RAISE LOG 'Migrating to: %', initial_version;

  -- Create maintenance schemas.
  CREATE SCHEMA maintenance;
  SET search_path TO maintenance;

  CREATE TABLE migrations
  ( migration     integer     PRIMARY KEY
  , at            timestamptz NOT NULL DEFAULT now()
  , to_version    text        NOT NULL
  , description   text        NOT NULL
  , in_progress   bool        NOT NULL
  );
  CREATE INDEX ON migrations (in_progress);

  CREATE VIEW migrations_in_progress AS
    SELECT *
    FROM migrations
    WHERE in_progress;

  CREATE VIEW latest_migration AS
    SELECT *
    FROM migrations
    ORDER BY migration DESC
    LIMIT 1;

  --------------------------------------------------------------------------

  CREATE FUNCTION maintenance.db_version() RETURNS text AS $fun$
  DECLARE
    version text;
  BEGIN
    SELECT to_version INTO STRICT version
    FROM maintenance.latest_migration;
    RETURN version;
  END;
  $fun$ LANGUAGE plpgsql STRICT STABLE;
  COMMENT ON FUNCTION maintenance.db_version() IS
    'The version of the latest database migration.';

  --------------------------------------------------------------------------

  CREATE FUNCTION maintenance.clean_db_version() RETURNS text AS $fun$
  BEGIN
    IF EXISTS (SELECT * FROM maintenance.migrations_in_progress) THEN
      RAISE EXCEPTION 'DB version is not clean, as there are migrations in progress.'
            USING HINT = 'Check the table ''maintenance.migrations''.';
    END IF;
    RETURN maintenance.db_version();
  END;
  $fun$ LANGUAGE plpgsql STRICT STABLE;
  COMMENT ON FUNCTION maintenance.db_version() IS
    'The version of the latest database migration. Throws an exception if a
    migration is still in progress.';

  --------------------------------------------------------------------------

  CREATE FUNCTION maintenance.start_migration
    (start_version text, initial_version text, _description text)
    RETURNS void AS $fun$
  DECLARE
    version text := maintenance.db_version();
    last_migration integer;
  BEGIN
    IF EXISTS (SELECT * FROM maintenance.migrations_in_progress) THEN
      RAISE EXCEPTION 'Cannot migrate, as there are migrations in progress.'
            USING HINT = 'Check the table ''maintenance.migrations''.';
    ELSE
      IF version = start_version THEN
        RAISE LOG 'Migrating from % to %', start_version, initial_version;
        SELECT max(migration)
        INTO   STRICT last_migration
        FROM   maintenance.migrations;

        INSERT INTO maintenance.migrations
               ( migration   -- note that we are not using a serial, because
                             -- we want concurrent migration-transactions to
                             -- fail, provided they are run in
                             -- 'serializable' mode.
               , to_version
               , description
               , in_progress
               )
        VALUES ( last_migration + 1
               , initial_version
               , _description
               , true
               );
      ELSE
        RAISE LOG 'DB version: %', version;
        RAISE EXCEPTION ' SKIPPING migration: % --> %', start_version, initial_version;
      END IF;
    END IF;
  END;
  $fun$ LANGUAGE plpgsql STRICT;
  COMMENT ON FUNCTION maintenance.start_migration(text,text,text) IS
    'Start a migration from a start version of a schema to another version.
     Call this function in a transaction that performs the whole migration.
     The function will throw an exception if the DB schema version does not
     match the start version; thereby aborting the migration transaction. You
     MUST call ''maintenance.finish_migration()'' to mark the migration as
     finished.';

  --------------------------------------------------------------------------

  CREATE FUNCTION maintenance.finish_migration() RETURNS void AS $fun$
  DECLARE
    lm maintenance.latest_migration%ROWTYPE;
  BEGIN
    IF EXISTS (SELECT * FROM maintenance.migrations_in_progress OFFSET 1) THEN
      RAISE EXCEPTION 'Cannot finish migration, as there is more than one migration in progress.'
            USING HINT = 'Check the table ''maintenance.migrations''.';
    ELSE
      SELECT *
      INTO   STRICT lm
      FROM   maintenance.latest_migration;

      IF lm.in_progress THEN
        UPDATE maintenance.migrations
        SET    in_progress = false
        WHERE  migration = lm.migration;
        RAISE LOG 'Migrated to: %', lm.to_version;
      ELSE
        RAISE EXCEPTION 'Cannot finish migration to %, as the latest migration is no longer in progress.', lm.to_version
            USING HINT = 'Check the table ''maintenance.migrations''.';
      END IF;
    END IF;
  END;
  $fun$ LANGUAGE plpgsql STRICT;
  COMMENT ON FUNCTION maintenance.finish_migration() IS
    'Mark that the migration started with
    ''maintenance.start_migration(text,text,text)'' is finished.';


  ----------------------------------------------------------------------------
  -- Finish initialization of migration table support
  ----------------------------------------------------------------------------

  -- Ensure that search path is safe again
  SET search_path TO public, pg_temp;

  -- Create first migration entry.
  INSERT INTO maintenance.migrations
              ( migration
              , to_version
              , description
              , in_progress
              )
  VALUES ( 0
         , initial_version
         , 'Initialized DB maintenance functionality.'
         , false
         );

  RAISE LOG 'Migrated to: %', maintenance.db_version();

EXCEPTION
  WHEN raise_exception THEN
    IF left(ltrim(SQLERRM),8) = 'SKIPPING' THEN
      RAISE NOTICE '%', SQLERRM;
    ELSE
      RAISE EXCEPTION 'Migration failed: %', SQLERRM;
    END IF;
END;
$$ LANGUAGE plpgsql;

END;
