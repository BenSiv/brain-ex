#!/usr/bin/env bats

# Define path to local binary
BREX="./bld/brex"

setup() {
    rm -rf tmp_vault_integrity
    rm -f tmp_integrity.db
    mkdir -p tmp_vault_integrity
    # We initialize properly to have a valid baseline
    $BREX init --vault tmp_vault_integrity --name tmp_integrity --editor touch
}

teardown() {
    rm -rf tmp_vault_integrity
    rm -f tmp_integrity.db
}

@test "database integrity: basic insert and select" {
    # This verifies that the 'database.lua' embedded in brex is functioning 
    # and doesn't have corrupted strings like "SE O" instead of "INSERT INTO"
    
    run $BREX tmp_integrity note --content "Integrity Check 1"
    if [ "$status" -ne 0 ]; then
        echo "Status: $status"
        echo "Output: $output"
    fi
    [ "$status" -eq 0 ]
    
    COUNT=$(sqlite3 tmp_integrity.db "SELECT COUNT(*) FROM notes WHERE content LIKE '%Integrity Check 1%';")
    [ "$COUNT" -eq 1 ]
}

@test "database integrity: handle special characters" {
    # Tests that quoting/escaping in database.lua is working (escape_sqlite)
    TEXT="Text with 'quotes' and \"double quotes\" and symbols !@#$%"
    run $BREX tmp_integrity note --content "$TEXT"
    [ "$status" -eq 0 ]

    COUNT=$(sqlite3 tmp_integrity.db "SELECT COUNT(*) FROM notes WHERE content LIKE '%Text with ''quotes''%';")
    [ "$COUNT" -eq 1 ]
}

@test "error handling: valid error message on failure" {
    # We want to force a database error.
    # We can use sqlite3 to drop the notes table, then run brex note.
    # This should trigger the "Invalid query" path in database.lua
    
    sqlite3 tmp_integrity.db "DROP TABLE notes;"
    
    run $BREX tmp_integrity note --content "Should fail"
    
    # It should fail
    [ "$status" -ne 0 ]
    
    # It should NOT print "nvalid query" (corrupted string)
    # It SHOULD print "Invalid query" and hopefully "no such table"
    [[ "$output" =~ "Invalid query" ]]
    [[ "$output" =~ "no such table" ]]
}

@test "init creates correct schema" {
    # Verify schema has correct columns
    SCHEMA=$(sqlite3 tmp_integrity.db ".schema notes")
    [[ "$SCHEMA" =~ "CREATE TABLE notes" ]]
    [[ "$SCHEMA" =~ "subject" ]]
    [[ "$SCHEMA" =~ "title" ]]
    [[ "$SCHEMA" =~ "content" ]]
}
