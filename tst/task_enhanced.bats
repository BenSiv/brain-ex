#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    mkdir -p "$HOME"
    export PATH="$PWD/bin:$PATH"
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    cleanup_test_env
}

@test "task with overdue date sets overdue flag" {
    # Add task with past due date
    run brex task add --content "Overdue task" --due_to "2020-01-01"
    [ "$status" -eq 0 ]
    
    # Check overdue flag is set
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE content='Overdue task';")
    [ "$OVERDUE" -eq 1 ]
}

@test "task default adds task without subcommand" {
    run brex task --content "Default add task"
    [ "$status" -eq 0 ]
    
    run brex task list
    [[ "$output" =~ "Default add task" ]]
}

@test "task done with comment stores comment" {
    brex task add --content "Task with comment"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM tasks LIMIT 1;")
    
    run brex task done --id "$TASK_ID" --comment "Completed successfully"
    [ "$status" -eq 0 ]
    
    COMMENT=$(sqlite3 tmp_vault.db "SELECT comment FROM tasks WHERE id='$TASK_ID';")
    [[ "$COMMENT" =~ "Completed successfully" ]]
}

@test "task done updates done timestamp" {
    brex task add --content "Check timestamp"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM tasks LIMIT 1;")
    
    brex task done --id "$TASK_ID"
    
    # Check done timestamp is not NULL
    DONE=$(sqlite3 tmp_vault.db "SELECT done FROM tasks WHERE id='$TASK_ID';")
    [ ! -z "$DONE" ]
}

@test "task list shows tasks ordered by due date" {
    brex task add --content "Task C" --due_to "2025-12-31"
    brex task add --content "Task A" --due_to "2025-10-15"
    brex task add --content "Task B" --due_to "2025-11-20"
    
    run brex task list
    [ "$status" -eq 0 ]
    
    # Extract just the content column and check ordering
    # Task A (Oct) should appear before Task B (Nov) before Task C (Dec)
    echo "$output" | grep -o "Task [ABC]" | head -n1 | grep "Task A"
}

@test "task add without due_to stores NULL in database" {
    run brex task add --content "No due date specified"
    [ "$status" -eq 0 ]
    
    # Check that a due_to is stored as NULL
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE content='No due date specified';")
    [ -z "$DUE_TO" ]
}

@test "task delay indefinitely clears due_to and overdue" {
    brex task add --content "Indefinite task" --due_to "2020-01-01"
    
    # Check that it starts with due_to and is overdue
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM tasks WHERE content='Indefinite task';")
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE id='$TASK_ID';")
    [ "$OVERDUE" -eq 1 ]
    
    # Run delay indefinitely
    run brex task delay --id "$TASK_ID" --due_to indefinitely
    [ "$status" -eq 0 ]
    
    # Check that due_to is cleared and overdue is 0
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE id='$TASK_ID';")
    [ -z "$DUE_TO" ]
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE id='$TASK_ID';")
    [ "$OVERDUE" -eq 0 ]
}

