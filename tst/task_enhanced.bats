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

@test "task delay indefinitely clears due_to and overdue" {
    brex task add --title "Indefinite task" --due_to "2020-01-01"

    # Check that it starts with due_to and is overdue
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM notes WHERE title='Indefinite task';")
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE item_id='$TASK_ID';")
    [ "$OVERDUE" -eq 1 ]

    # Run delay indefinitely
    run brex task delay --id "$TASK_ID" --due_to indefinitely
    [ "$status" -eq 0 ]

    # Check that due_to is cleared and overdue is 0
    DUE_TO=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks WHERE item_id='$TASK_ID';")
    [ -z "$DUE_TO" ]
    OVERDUE=$(sqlite3 tmp_vault.db "SELECT overdue FROM tasks WHERE item_id='$TASK_ID';")
    [ "$OVERDUE" -eq 0 ]
}

@test "settings.json task_columns selection is respected" {
    # Write custom settings.json
    mkdir -p "$HOME/.config/brain-ex"
    cat <<EOF > "$HOME/.config/brain-ex/settings.json"
{
  "task_columns": ["id", "subject", "owner"]
}
EOF

    brex task add --title "Test task content" --importance 5 --urgency 5 --owner "test-owner"

    run brex task list
    [ "$status" -eq 0 ]

    # "owner" column should be visible, but "priority" or "title" should NOT be visible.
    [[ "$output" =~ "test-owner" ]]
    [[ ! "$output" =~ "priority" ]]
    [[ ! "$output" =~ "Test task content" ]]
}

@test "settings.json custom quadrant colors are respected" {
    # Write custom settings.json with a unique ANSI color code for Q1 (e.g. \u001b[96m for cyan)
    mkdir -p "$HOME/.config/brain-ex"
    cat <<EOF > "$HOME/.config/brain-ex/settings.json"
{
  "task_columns": ["id", "subject"],
  "colors": {
    "Q1": "\u001b[96m",
    "reset": "\u001b[0m"
  }
}
EOF

    # Q1 is imp >= 4 and urg >= 4
    brex task add --title "Cyan task" --importance 5 --urgency 5

    run brex task list
    [ "$status" -eq 0 ]

    # The output should contain the cyan escape sequence \027[96m or \e[96m
    # In bash/bats, we can match against the escape character \x1b or \033 or \e
    [[ "$output" =~ $'\e[96m' ]]
}

@test "deprecation warning for hide_due_to in config.yaml is shown" {
    # Enable hide_due_to in config.yaml
    echo "hide_due_to: true" >> "$HOME/.config/brain-ex/config.yaml"

    brex task add --title "Warning check task"

    run brex task list
    [ "$status" -eq 0 ]

    # Deprecation warning should be printed on stderr
    [[ "$output" =~ "Warning: 'hide_due_to' configuration in config.yaml is deprecated" ]]
}
