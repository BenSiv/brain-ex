#!/usr/bin/env bats

load test_helper.bash
BREX="brex"

resolve_brex() {
    if [ -x "$PROJECT_ROOT/bin/brex" ]; then
        BREX="$PROJECT_ROOT/bin/brex"
    else
        BREX="brex"
    fi
}

setup() {
    setup_test_env
    resolve_brex
    mkdir -p "$HOME"
    rm -rf tmp_vault_integrity
    rm -f tmp_integrity.db
    mkdir -p tmp_vault_integrity
    # We initialize properly to have a valid baseline
    $BREX init --vault tmp_vault_integrity --name tmp_integrity --editor touch
}

teardown() {
    rm -rf tmp_vault_integrity
    rm -f tmp_integrity.db
    cleanup_test_env
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

    CONTENT=$(sqlite3 tmp_integrity.db "SELECT content FROM notes WHERE content LIKE '%Text with%';")
    [[ "$CONTENT" =~ "Text with quotes and double quotes" ]]
}

@test "error handling: valid error message on failure" {
    # Vault-backed brains should self-heal if the local DB schema disappears.
    
    sqlite3 tmp_integrity.db "DROP TABLE notes;"
    
    run $BREX tmp_integrity note --content "Should fail"
    [ "$status" -eq 0 ]

    COUNT=$(sqlite3 tmp_integrity.db "SELECT COUNT(*) FROM notes WHERE content LIKE '%Should fail%';")
    [ "$COUNT" -eq 1 ]
}

@test "init creates correct schema" {
    # Verify schema has correct columns
    SCHEMA=$(sqlite3 tmp_integrity.db ".schema notes")
    [[ "$SCHEMA" =~ "CREATE TABLE notes" ]]
    [[ "$SCHEMA" =~ "subject" ]]
    [[ "$SCHEMA" =~ "title" ]]
    [[ "$SCHEMA" =~ "content" ]]
}
