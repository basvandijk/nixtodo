BEGIN;

DO $block$
BEGIN

  PERFORM maintenance.start_migration( '1', '2', 'events');

  CREATE FUNCTION send_event() RETURNS trigger AS $send_event$
      BEGIN
          IF TG_OP = 'DELETE'
          THEN
            EXECUTE pg_notify('event_channel', TG_OP || ':' || OLD.id);
          ELSE
            EXECUTE pg_notify('event_channel', TG_OP || ':' || NEW.id);
          END IF;
          RETURN NULL;
      END;
  $send_event$ LANGUAGE plpgsql;

  CREATE TRIGGER send_event_trigger
    AFTER INSERT OR UPDATE OR DELETE
    ON entries
    FOR EACH ROW
    EXECUTE PROCEDURE send_event();

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
