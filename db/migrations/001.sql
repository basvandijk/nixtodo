BEGIN;

DO $block$
BEGIN

  PERFORM maintenance.start_migration( '0', '1', 'Add entries table');

  CREATE TABLE entries
  ( id          serial
  , description text   NOT NULL
  , completed   bool   NOT NULL
  , CONSTRAINT pk_entries PRIMARY KEY (id)
  );

  PERFORM maintenance.finish_migration();

EXCEPTION
  WHEN raise_exception THEN
    IF left(ltrim(SQLERRM),8) = 'SKIPPING' THEN
      RAISE NOTICE '%', SQLERRM;
    ELSE
      RAISE EXCEPTION 'Migration failed: %', SQLERRM;
    END IF;
END
$block$ LANGUAGE plpgsql;

END;
