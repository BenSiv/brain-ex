#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"

setup() {
    rm -f brain.db work.db personal.db
}

teardown() {
    rm -f brain.db work.db personal.db
    rm -f "$CONFIG"
}

@test "init default brain" {
    run brex init
    [ "$status" -eq 0 ]
    [ -f "brain.db" ]
    grep -q "brain: .*brain.db" "$CONFIG"
}

@test "init named brain (work)" {
    run brex init --name work
    [ "$status" -eq 0 ]
    [ -f "work.db" ]
    
    grep -q "brains:" "$CONFIG"
    grep -q "work: .*work.db" "$CONFIG"
}

@test "switch between brains using CLI" {
    # Initialize both
    run brex init
    run brex init --name work
    
    # Add task to default
    run brex task add -c "Default Task"
    [ "$status" -eq 0 ]
    
    # Add task to work
    run brex work task add -c "Work Task"
    [ "$status" -eq 0 ]
    
    # Verify content via valid command output checks
    run brex task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Default Task" ]]
    [[ ! "$output" =~ "Work Task" ]]

    run brex work task list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Work Task" ]]
    [[ ! "$output" =~ "Default Task" ]]
}

@test "error on invalid brain name" {
    run brex invalid_brain task list
    [ "$status" -eq 1 ]
    [[ "$output" =~ "Error: Brain 'invalid_brain' not configured" ]]
}
