#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    mkdir -p "$HOME"
    export PATH="$PWD/bin:$PATH"
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    cleanup_test_env
}

@test "view displays NULL values as empty strings" {
    # Create task with no subject (defaults to NULL)
    brex task add --content "Task with missing subject"
    
    run brex task list
    echo "Output: $output"
    [ "$status" -eq 0 ]
    
    # "NULL" string should not appear in output
    [[ ! "$output" =~ "NULL" ]]
    # Content should appear
    [[ "$output" =~ "Task with missing subject" ]]
}
