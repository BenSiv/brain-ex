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

@test "task add without due_to defaults to tomorrow" {
    run brex task add --content "No due date specified"
    [ "$status" -eq 0 ]
    
    # Check that a due_to was set automatically
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE content='No due date specified';")
    [ ! -z "$DUE_TO" ]
    # Should not be NULL and should be a timestamp format
    [[ "$DUE_TO" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2} ]]
}
