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
    run brex task add --title "Deploy new patch" --subject "backend" --due_to "2025-08-15"
    [ "$status" -eq 0 ]

    run brex task list
    [[ "$output" =~ "Deploy new patch" ]]
}

@test "mark task as done and list should be empty" {
    brex task add --title "Deploy new patch" --subject "backend"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Deploy new patch';")

    run brex task done --id "$TASK_ID" --comment "Done"
    [ "$status" -eq 0 ]

    # Now check the open task list
    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "No pending tasks" ]]   # ensures list is empty
}

@test "NULL values are parsed as empty string in task list" {
    run brex task add --title "Check null parsing"
    [ "$status" -eq 0 ]

    # Manually set due_to to NULL
    sqlite3 tmp_vault.db "UPDATE tasks SET due_to=NULL WHERE item_id=(SELECT id FROM notes WHERE title='Check null parsing');"

    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Check null parsing" ]]
    [[ "$output" =~ "due_to" ]]  # Should show blank/empty field
}

@test "task list with --subject filters tasks" {
    brex task add --title "Backend task" --subject "backend"
    brex task add --title "Frontend task" --subject "frontend"
    brex task add --title "DevOps task" --subject "devops"

    run brex task list --subject "backend"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Backend task" ]]
    [[ ! "$output" =~ "Frontend task" ]]
    [[ ! "$output" =~ "DevOps task" ]]
}

@test "task with overdue date sets overdue flag" {
    # Add task with past due date
    run brex task add --title "Overdue task" --due_to "2020-01-01"
    [ "$status" -eq 0 ]

    # Check overdue flag is set
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE item_id=(SELECT id FROM notes WHERE title='Overdue task');")
    [ "$OVERDUE" -eq 1 ]
}

@test "task default adds task without subcommand" {
    run brex task --title "Default add task"
    [ "$status" -eq 0 ]

    run brex task list
    [[ "$output" =~ "Default add task" ]]
}

@test "task done with comment stores comment" {
    brex task add --title "Task with comment"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Task with comment';")

    run brex task done --id "$TASK_ID" --comment "Completed successfully"
    [ "$status" -eq 0 ]

    # There is no separate comment column any more -- the done comment
    # is just the last entry appended to the task's own note content.
    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE id='$TASK_ID';")
    [[ "$CONTENT" =~ "Completed successfully" ]]
}

@test "task done updates done timestamp" {
    brex task add --title "Check timestamp"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Check timestamp';")

    brex task done --id "$TASK_ID"

    # Check done timestamp is not NULL
    DONE=$(sqlite3 tmp_vault.db "SELECT done FROM tasks WHERE item_id='$TASK_ID';")
    [ ! -z "$DONE" ]
}

@test "task list shows tasks ordered by due date" {
    brex task add --title "Task C" --due_to "2025-12-31"
    brex task add --title "Task A" --due_to "2025-10-15"
    brex task add --title "Task B" --due_to "2025-11-20"

    run brex task list
    [ "$status" -eq 0 ]

    # Extract just the title column and check ordering
    # Task A (Oct) should appear before Task B (Nov) before Task C (Dec)
    echo "$output" | grep -o "Task [ABC]" | head -n1 | grep "Task A"
}

@test "task add without due_to stores NULL in database" {
    run brex task add --title "No due date specified"
    [ "$status" -eq 0 ]

    # Check that a due_to is stored as NULL
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE item_id=(SELECT id FROM notes WHERE title='No due date specified');")
    [ -z "$DUE_TO" ]
}

@test "task list with --due_to shows only tasks after date" {
    brex task add --title "Early task" --due_to "2025-01-15"
    brex task add --title "Late task" --due_to "2025-12-31"

    run brex task list --due_to "2025-06-01"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Late task" ]]
    [[ ! "$output" =~ "Early task" ]]
}

@test "tasks markdown edits are synchronized before the next task command" {
    mkdir -p tmp_vault/ops
    cat <<EOF > "tmp_vault/ops/From Markdown.md"
---
id: 701
title: "From Markdown"
is_task: true
time: "2026-05-01 10:00:00"
due_to: "2026-05-03 10:00:00"
overdue: 0
importance: 1
urgency: 1
---
From Markdown body
EOF

    # Run update to sync manual edits before running read-only command
    run brex update
    [ "$status" -eq 0 ]

    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "From Markdown" ]]

    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes JOIN tasks ON tasks.item_id = notes.id WHERE notes.title='From Markdown' AND notes.subject='ops';")
    [ "$COUNT" -eq 1 ]
}

@test "task list with --owner filters agent tasks" {
    brex task add --title "Agent owned task" --subject "ops" --due_to "2026-05-03" --owner "agent"
    brex task add --title "User task" --subject "ops" --due_to "2026-05-03" --owner "user"

    run brex task list --owner agent
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Agent owned task" ]]
    [[ ! "$output" =~ "User task" ]]
}

@test "task add with invalid due_to returns error" {
    run brex task add --title "Invalid task" --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "task list with invalid due_to returns error" {
    brex task add --title "Valid task"
    run brex task list --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "task delay with invalid due_to returns error" {
    brex task add --title "Task to delay"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Task to delay';")
    run brex task delay --id "$TASK_ID" --due_to "Friday"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Due To must conform to time-stamp format" ]]
}

@test "task comment appends without marking done" {
    brex task add --title "Ongoing investigation"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Ongoing investigation';")

    run brex task comment --id "$TASK_ID" --comment "Found the root cause"
    [ "$status" -eq 0 ]

    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE id='$TASK_ID';")
    [[ "$CONTENT" =~ "Found the root cause" ]]

    # Still pending -- a comment must not mark it done
    run brex task list
    [[ "$output" =~ "Ongoing investigation" ]]
}

@test "task show displays title and full content log" {
    brex task add --title "Show me" --content "initial body"
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Show me';")
    brex task comment --id "$TASK_ID" --comment "a later update"

    run brex task show --id "$TASK_ID"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Show me" ]]
    [[ "$output" =~ "initial body" ]]
    [[ "$output" =~ "a later update" ]]
}

@test "task add promotes an existing note instead of duplicating it" {
    brex note add --subject "benchling" --title "some finding" --content "root cause found"
    run brex task add --subject "benchling" --title "some finding" --due_to "2026-09-05" --importance 3
    [ "$status" -eq 0 ]

    NOTE_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE subject='benchling' AND title='some finding';")
    [ "$NOTE_COUNT" -eq 1 ]

    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE subject='benchling' AND title='some finding';")
    [[ "$CONTENT" =~ "root cause found" ]]

    run brex task list
    [[ "$output" =~ "some finding" ]]
}
