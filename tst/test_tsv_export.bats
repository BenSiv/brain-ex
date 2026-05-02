#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    mkdir -p "$HOME"
    export PATH="$PWD/bin:$PATH"
    rm -rf tmp_vault_tsv
    rm -f tmp_vault_tsv.db
}

teardown() {
    rm -rf tmp_vault_tsv
    rm -f tmp_vault_tsv.db
    cleanup_test_env
}

@test "backup tsv export has correct headers and includes comment" {
    # setup vault and brain
    run brex init --vault tmp_vault_tsv
    if [ "$status" -ne 0 ]; then
        echo "Init failed: $output"
    fi
    [ "$status" -eq 0 ]
    
    # add task
    run brex task -d add -s "Test Task" -c "Content"
    echo "Add Task Output: $output"
    if [ "$status" -ne 0 ]; then
        echo "Task add failed: $output"
    fi
    [ "$status" -eq 0 ]
    
    # check tasks.tsv exists
    [ -f "tmp_vault_tsv/tasks.tsv" ]
    
    # Read first line (header)
    header=$(head -n 1 tmp_vault_tsv/tasks.tsv)
    echo "Header: $header"
    
    # Assert header contains "subject" and "comment"
    [[ "$header" == *"subject"* ]]
    [[ "$header" == *"comment"* ]]
    
    # Assert header does NOT contain number "1"
    [[ "$header" != "1"* ]]
}
