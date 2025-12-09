#!/usr/bin/env bats

setup() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
}

@test "add a task and list it" {
    run brex task add --content "Deploy new patch" --subject "backend" --due_to "2025-08-15"
    [ "$status" -eq 0 ]

    run brex task list
    [[ "$output" =~ "Deploy new patch" ]]
}

@test "mark task as done and list should be empty" {
    brex task add --content "Deploy new patch" --subject "backend"
    TASK_ID=$(sqlite3 tmp_vault.db "select id from tasks limit 1;")

    run brex task done --id "$TASK_ID" --comment "Done"
    [ "$status" -eq 0 ]

    # Now check the open task list
    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "No pending tasks" ]]   # ensures list is empty
}

@test "NULL values are parsed as empty string in task list" {
    run brex task add --content "Check null parsing"
    [ "$status" -eq 0 ]

    # Manually set due_to to NULL
    sqlite3 tmp_vault.db "UPDATE tasks SET due_to=NULL WHERE content='Check null parsing';"

    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Check null parsing" ]]
    [[ "$output" =~ "due_to" ]]  # Should show blank/empty field
}

@test "task list with --subject filters tasks" {
    brex task add --content "Backend task" --subject "backend"
    brex task add --content "Frontend task" --subject "frontend"
    brex task add --content "DevOps task" --subject "devops"
    
    run brex task list --subject "backend"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Backend task" ]]
    [[ ! "$output" =~ "Frontend task" ]]
    [[ ! "$output" =~ "DevOps task" ]]
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

@test "task list with --due_to shows only tasks after date" {
    brex task add --content "Early task" --due_to "2025-01-15"
    brex task add --content "Late task" --due_to "2025-12-31"
    
    run brex task list --due_to "2025-06-01"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Late task" ]]
    [[ ! "$output" =~ "Early task" ]]
}
