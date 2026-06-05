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

@test "task add without due_to stores NULL in database" {
    run brex task add --content "No due date specified"
    [ "$status" -eq 0 ]
    
    # Check that a due_to is stored as NULL
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE content='No due date specified';")
    [ -z "$DUE_TO" ]
}

@test "task list with --due_to shows only tasks after date" {
    brex task add --content "Early task" --due_to "2025-01-15"
    brex task add --content "Late task" --due_to "2025-12-31"
    
    run brex task list --due_to "2025-06-01"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Late task" ]]
    [[ ! "$output" =~ "Early task" ]]
}

@test "tasks markdown edits are synchronized before the next task command" {
    mkdir -p tmp_vault/tasks
    cat <<EOF > tmp_vault/tasks/701.md
---
id: "701"
time: "2026-05-01 10:00:00"
subject: "ops"
due_to: "2026-05-03 10:00:00"
overdue: 0
importance: 1
urgency: 1
---
From Markdown
EOF

    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "From Markdown" ]]

    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM tasks WHERE id=701 AND content='From Markdown';")
    [ "$COUNT" -eq 1 ]
}

@test "task list with --owner filters agent tasks" {
    brex task add --content "Agent owned task" --subject "ops" --due_to "2026-05-03" --owner "agent"
    brex task add --content "User task" --subject "ops" --due_to "2026-05-03" --owner "user"

    run brex task list --owner agent
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Agent owned task" ]]
    [[ ! "$output" =~ "User task" ]]
}

@test "task add with invalid due_to returns error" {
    run brex task add --content "Invalid task" --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "task list with invalid due_to returns error" {
    brex task add --content "Valid task"
    run brex task list --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "task delay with invalid due_to returns error" {
    brex task add --content "Task to delay"
    TASK_ID=$(sqlite3 tmp_vault.db "select id from tasks limit 1;")
    run brex task delay --id "$TASK_ID" --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}
