#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    mkdir -p "$HOME"
    export PATH="$PWD/bin:$PATH"
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir -p tmp_vault
    brex init --vault tmp_vault --editor touch
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    cleanup_test_env
}

@test "handle very long note content (>10KB)" {
    # Create a very long content string (about 15KB)
    LONG_CONTENT=$(python3 -c "print('A' * 15000)")
    
    run brex note add --title "long-note" --content "$LONG_CONTENT" --subject "test"
    [ "$status" -eq 0 ]
    
    # Verify it was stored
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='long-note';")
    [ "$COUNT" -eq 1 ]
}

@test "handle special characters and quotes in SQL" {
    # Test with content that could break SQL if not properly escaped
    run brex note add --title "sql-test" --content "Content with 'single' and \"double\" and \`backticks\`" --subject "test"
    [ "$status" -eq 0 ]
    
    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE title='sql-test';")
    [[ "$CONTENT" =~ "single" ]]
}

@test "multiple rapid note creations maintain uniqueness" {
    # Create multiple notes rapidly
    brex note add --title "rapid1" --content "Content 1" --subject "test" &
    brex note add --title "rapid2" --content "Content 2" --subject "test" &
    brex note add --title "rapid3" --content "Content 3" --subject "test" &
    wait
    
    # All three should exist
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE subject='test' AND title LIKE 'rapid%';")
    [ "$COUNT" -eq 3 ]
}

@test "invalid timestamp format for task due_to" {
    # Invalid timestamp should return an error
    run brex task add --title "Bad timestamp task" --due_to "invalid-date"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "missing config file shows helpful error" {
    # Remove config
    rm -f "$CONFIG"
    
    # Try to run a command that needs config
    run brex note add --title "test" --content "test" --subject "test"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "does not exist" || "$output" =~ "brex init" ]]
}

@test "subcommand help works without config" {
    # Remove config
    rm -f "$CONFIG"
    
    # Run help for a subcommand
    run brex task delay --help
    [ "$status" -eq 0 ]
    [[ "$output" != *"does not exist"* ]]
    [[ "$output" != *"brex init"* ]]
    [[ "$output" =~ "Usage: " && "$output" =~ "brex task delay" ]]
}

@test "unknown subcommand reports error message on help" {
    # Run unknown subcommand with help
    run brex task foo --help
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Unknown subcommand: foo" ]]
    [[ "$output" =~ "Available subcommands: add, list, done, delay, prioritize, comment, show, last" ]]
}
