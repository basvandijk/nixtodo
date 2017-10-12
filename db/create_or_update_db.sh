#!/bin/sh

set -o errexit

DB_NAME="${1}"

PORT="${2}"

export PGPASSWORD
PGPASSWORD="$(cat ${3})"

MIGRATIONS="${4}"

PSQL_NIXTODO="psql\
 --host=127.0.0.1\
 --port=${PORT}\
 --username=nixtodo\
 --no-psqlrc\
 --quiet\
 --set=ON_ERROR_STOP=on\
 --set=AUTOCOMMIT=off"

dbExists() {
    ${PSQL_NIXTODO} --list --tuples-only | \
     cut -d \| -f 1 | grep "^ *${1} *$" | wc -l
}

if [ $(dbExists "${DB_NAME}") == "0" ]; then
    (cat <<EOI
CREATE DATABASE ${DB_NAME}
  OWNER nixtodo
  ENCODING 'UTF8' TEMPLATE template0
  CONNECTION LIMIT 128;

ALTER DATABASE ${DB_NAME}
  SET default_transaction_isolation TO 'serializable';

EOI
) | ${PSQL_NIXTODO} template1
fi

cd ${MIGRATIONS};
for migration in *.sql
do
    ${PSQL_NIXTODO} ${DB_NAME} -f ${migration}
done
