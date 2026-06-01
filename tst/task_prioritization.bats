#!/usr/bin/env bats

setup() {
    export PATH="/home/bensiv/Projects/brain-ex/bin:$PATH"
    rm -rf tmp_vault
    rm -f tmp_vault.db
    rm -f "$HOME/.config/brain-ex/config.yaml"
    cleanup_test_env
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    rm -f "$HOME/.config/brain-ex/config.yaml"
    cleanup_test_env
}

cleanup_test_env() {
    # Delete test TSV and databases to avoid pollution
    rm -f tasks.tsv
    rm -f brain.db
}

@test "prioritization: add task with valid importance and urgency" {
    brex tmp_vault task add --content "Test task" --importance 4 --urgency 3

    # Check database fields
    IMP=$(sqlite3 tmp_vault.db "SELECT importance FROM tasks LIMIT 1;")
    [ "$IMP" -eq 4 ]
    URG=$(sqlite3 tmp_vault.db "SELECT urgency FROM tasks LIMIT 1;")
    [ "$URG" -eq 3 ]
}

@test "prioritization: validate importance bounds" {
    run brex tmp_vault task add --content "Test task" --importance 0
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Importance must be an integer between 1 and 5" ]]

    run brex tmp_vault task add --content "Test task" --importance 6
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Importance must be an integer between 1 and 5" ]]
}

@test "prioritization: validate urgency bounds" {
    run brex tmp_vault task add --content "Test task" --urgency 0
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Urgency must be an integer between 1 and 5" ]]

    run brex tmp_vault task add --content "Test task" --urgency 6
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Urgency must be an integer between 1 and 5" ]]
}

@test "prioritization: optional due date defaults to NULL" {
    brex tmp_vault task add --content "Test task"

    DUE=$(sqlite3 tmp_vault.db "SELECT due_to FROM tasks LIMIT 1;")
    [ -z "$DUE" ]
}

@test "prioritization: dynamic urgency boosts" {
    # Overdue task -> urgency 5
    brex tmp_vault task add --content "Overdue task" --due_to "2025-01-01 12:00:00" --importance 4 --urgency 1
    
    # Task due in far future -> manual urgency 3
    LATE_DATE=$(date -d "+10 days" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -v+10d "+%Y-%m-%d %H:%M:%S")
    brex tmp_vault task add --content "Far future task" --due_to "$LATE_DATE" --importance 4 --urgency 3

    # List tasks and check prioritization color output
    run brex tmp_vault task list
    [ "$status" -eq 0 ]

    # Overdue task: Importance 4, Active Urgency 5 -> Q1 (Bold Red)
    [[ "$output" =~ "Q1" ]]

    # Far future task: Importance 4, Active Urgency 3 -> Q2 (Bold Yellow/Gold)
    [[ "$output" =~ "Q2" ]]
}

@test "prioritization: sorting order Q1 -> Q3 -> Q2 -> Q4 -> NULL deadlines" {
    # 1. NULL deadline (Importance 1, Urgency 1) -> Q4, listed last
    brex tmp_vault task add --content "NULL deadline task"

    # 2. Q4 task with far due date (Importance 1, Urgency 1)
    LATE_DATE=$(date -d "+20 days" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -v+20d "+%Y-%m-%d %H:%M:%S")
    brex tmp_vault task add --content "Q4 task" --due_to "$LATE_DATE" --importance 1 --urgency 1

    # 3. Q2 task (Importance 5, Urgency 2)
    brex tmp_vault task add --content "Q2 task" --due_to "$LATE_DATE" --importance 5 --urgency 2

    # 4. Q1 task (overdue -> active urgency 5, Importance 5)
    brex tmp_vault task add --content "Q1 task" --due_to "2025-01-01 12:00:00" --importance 5 --urgency 1

    # 5. Q3 task (overdue -> active urgency 5, Importance 2)
    brex tmp_vault task add --content "Q3 task" --due_to "2025-01-01 12:00:00" --importance 2 --urgency 1

    run brex tmp_vault task list
    [ "$status" -eq 0 ]

    # Sorting order should be:
    # Q1 task (Score 25)
    # Q2 task (Score 10, Importance 5)
    # Q3 task (Score 10, Importance 2)
    # Q4 task (Score 1, Active Urgency 1, due first)
    # NULL deadline task (Score 1, Active Urgency 1, listed last)

    # Let's extract the order of tasks
    ORDERED_TASKS=$(echo "$output" | grep -oE "Q[1-4] task|NULL deadline task")
    EXPECTED=$(cat <<EOF
Q1 task
Q2 task
Q3 task
Q4 task
NULL deadline task
EOF
)
    [ "$ORDERED_TASKS" = "$EXPECTED" ]
}

@test "prioritization: hide due_to column via config setting" {
    # 1. By default, due_to is visible
    brex tmp_vault task add --content "Default task"
    run brex tmp_vault task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "due_to" ]]

    # 2. Set hide_due_to: true in config
    echo "hide_due_to: true" >> "$HOME/.config/brain-ex/config.yaml"

    # 3. due_to should be hidden
    run brex tmp_vault task list
    [ "$status" -eq 0 ]
    [[ ! "$output" =~ "due_to" ]]
}

@test "prioritization: update priority rankings" {
    brex tmp_vault task add --content "Rank update task" --importance 1 --urgency 1
    
    # Extract task ID
    TASK_ID=$(sqlite3 tmp_vault.db "SELECT id FROM tasks WHERE content='Rank update task';")
    [ -n "$TASK_ID" ]

    # Update ranking
    run brex tmp_vault task prioritize --id "$TASK_ID" --importance 5 --urgency 4
    [ "$status" -eq 0 ]

    # Verify update in database
    IMP=$(sqlite3 tmp_vault.db "SELECT importance FROM tasks WHERE id='$TASK_ID';")
    URG=$(sqlite3 tmp_vault.db "SELECT urgency FROM tasks WHERE id='$TASK_ID';")
    [ "$IMP" -eq 5 ]
    [ "$URG" -eq 4 ]

    # Verify invalid bounds checks
    run brex tmp_vault task prioritize --id "$TASK_ID" --importance 6
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Importance must be an integer between 1 and 5" ]]

    run brex tmp_vault task prioritize --id "$TASK_ID" --urgency 0
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Urgency must be an integer between 1 and 5" ]]
}


