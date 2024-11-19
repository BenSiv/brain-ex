#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <db_file> <tsv_file>"
    exit 1
fi

DB_FILE="$1"
TSV_FILE="$2"
TABLE_NAME="tasks"

sqlite3 "$DB_FILE" <<EOF
.mode tabs
.import $TSV_FILE $TABLE_NAME
.quit
EOF