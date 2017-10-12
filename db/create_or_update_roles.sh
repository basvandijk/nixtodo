#!/bin/sh

set -o errexit

nixtodoPassword="$(cat $1)"

hashedPassword() {
    role="$1"
    password="$2"
    echo -n 'md5'
    echo -n "$password$role" | md5sum | cut -d' ' -f1
}

roleExists() {
    psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='$1'" | grep -q 1
}

createOrAlterRole() {
    if roleExists "$1"; then
        echo "ALTER ROLE $1"
    else
        echo "CREATE ROLE $1"
    fi
}

psql postgres --command="$(createOrAlterRole "nixtodo") \
                          SUPERUSER LOGIN PASSWORD \
                          '$(hashedPassword "nixtodo" "$nixtodoPassword")';"
