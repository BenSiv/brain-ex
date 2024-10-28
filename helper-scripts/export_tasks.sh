#!/bin/bash

# Variables
DB_FILE="../obsidian-work.db"
OUTPUT_FILE="tasks.tsv"
SQL_QUERY="SELECT * FROM todos;"

# Run the SQLite command
sqlite3 "$DB_FILE" <<EOF
.headers on
.mode tabs
.output $OUTPUT_FILE
$SQL_QUERY
.output stdout
.quit
EOF

echo "Data exported to $OUTPUT_FILE"
