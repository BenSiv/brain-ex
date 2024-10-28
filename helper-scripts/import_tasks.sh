#!/bin/bash

# Variables
DB_FILE="../obsidian-work.db"  # Path to the SQLite database
TSV_FILE="tasks.tsv"           # Path to the TSV file to import
TABLE_NAME="todos"             # Table to import the data into

# Run the SQLite command
sqlite3 "$DB_FILE" <<EOF
.mode tabs
.import $TSV_FILE $TABLE_NAME
.quit
EOF

echo "Data imported from $TSV_FILE into $TABLE_NAME table"
