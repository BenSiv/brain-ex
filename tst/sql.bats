#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"

setup() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    rm -f "$CONFIG"
}

@test "sql shell launches interactive sqlite3" {
    # We can't easily test interactive mode, but we can verify the command runs
    # by checking it exits cleanly when stdin is closed
    run bash -c "echo '' | brex sql"
    [ "$status" -eq 0 ]
}

@test "sql with query flag executes without error" {
    # Add a note to have some data
    brex note add --title "test-note" --content "Test content" --subject "test"
    
    # Just verify it runs without error
    run brex sql --query "SELECT COUNT(*) FROM notes;"
    [ "$status" -eq 0 ]
}

@test "sql query on empty table runs without error" {
    # Query empty tasks table
    run brex sql --query "SELECT * FROM tasks;"
    [ "$status" -eq 0 ]
}

@test "sql opens interactive shell by default" {
    # Test that sql command without --query opens shell
    run bash -c "echo '.quit' | brex sql"
    [ "$status" -eq 0 ]
}
