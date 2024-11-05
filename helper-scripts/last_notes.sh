#!/bin/bash

# Check if an argument is passed
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <number_of_notes>"
    exit 1
fi

# Get the number of notes from the argument
num_notes=$1

# Run the SQLite query with the provided limit
sqlite3 obsidian-work.db -column -header "SELECT name, content FROM notes WHERE [group]='daily-notes' ORDER BY name DESC LIMIT $num_notes;"
