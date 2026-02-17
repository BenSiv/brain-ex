#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"
BREX="brex"

resolve_brex() {
    if [ -x "./bin/brex" ]; then
        BREX="./bin/brex"
    else
        BREX="brex"
    fi
}

setup() {
    resolve_brex
    rm -f brain.db work.db personal.db
}

teardown() {
    rm -f brain.db work.db personal.db
    rm -f "$CONFIG"
}

@test "init default brain" {
    run $BREX init
    [ "$status" -eq 0 ]
    [ -f "brain.db" ]
    grep -q "brain: .*brain.db" "$CONFIG"
}

@test "init named brain (work)" {
    run $BREX init --name work
    [ "$status" -eq 0 ]
    [ -f "work.db" ]
    
    grep -q "brains:" "$CONFIG"
    grep -q "work: .*work.db" "$CONFIG"
}

@test "switch between brains using CLI" {
    # Initialize both
    run $BREX init
    run $BREX init --name work
    
    # Add task to default
    run $BREX task add -c "Default Task"
    [ "$status" -eq 0 ]
    
    # Add task to work
    run $BREX work task add -c "Work Task"
    [ "$status" -eq 0 ]
    
    # Verify content via valid command output checks
    run $BREX task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Default Task" ]]
    [[ ! "$output" =~ "Work Task" ]]

    run $BREX work task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Work Task" ]]
    [[ ! "$output" =~ "Default Task" ]]
}

@test "brain list shows configured named brains" {
    run $BREX init
    [ "$status" -eq 0 ]

    run $BREX init --name work
    [ "$status" -eq 0 ]

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Default brain" ]]
    [[ "$output" =~ "work" ]]
}

@test "brain use switches default brain" {
    run $BREX init
    [ "$status" -eq 0 ]

    run $BREX init --name work
    [ "$status" -eq 0 ]

    run $BREX brain use --name work
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Default brain set to 'work'" ]]

    run $BREX task add -c "Task on switched default"
    [ "$status" -eq 0 ]

    DEFAULT_COUNT=$(sqlite3 brain.db "SELECT COUNT(*) FROM tasks WHERE content='Task on switched default';")
    WORK_COUNT=$(sqlite3 work.db "SELECT COUNT(*) FROM tasks WHERE content='Task on switched default';")

    [ "$DEFAULT_COUNT" -eq 0 ]
    [ "$WORK_COUNT" -eq 1 ]
}

@test "error on invalid brain name" {
    run $BREX invalid_brain task list
    [ "$status" -eq 1 ]
    [[ "$output" =~ "Error: Brain 'invalid_brain' not configured" ]]
}
