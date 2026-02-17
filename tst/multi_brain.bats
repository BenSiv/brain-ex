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
    rm -rf tmp_notes
    rm -rf nested
    rm -f brain.db work.db personal.db tmp_notes.db work_notes.db
}

teardown() {
    rm -rf tmp_notes
    rm -rf nested
    rm -f brain.db work.db personal.db tmp_notes.db work_notes.db
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

@test "init with matching vault and name appears in brain list" {
    mkdir -p tmp_notes

    run $BREX init --vault tmp_notes --name tmp_notes
    [ "$status" -eq 0 ]

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "tmp_notes" ]]
}

@test "init with vault and no name uses vault folder as label" {
    mkdir -p tmp_notes

    run $BREX init --vault tmp_notes
    [ "$status" -eq 0 ]

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Default brain: tmp_notes" ]]
    [[ "$output" =~ "tmp_notes ->" ]]
}

@test "init with vault and custom name uses provided name as label" {
    mkdir -p tmp_notes

    run $BREX init --vault tmp_notes --name work_notes
    [ "$status" -eq 0 ]

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "work_notes ->" ]]
    [[ ! "$output" =~ "tmp_notes ->" ]]
}

@test "init with vault path ending slash uses basename as label" {
    mkdir -p nested/tmp_notes

    run $BREX init --vault nested/tmp_notes/
    [ "$status" -eq 0 ]

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "tmp_notes ->" ]]
}

@test "brain list infers label for legacy config without brains map" {
    mkdir -p "$(dirname "$CONFIG")"
    cat > "$CONFIG" <<'EOF'
brain: /tmp/legacy.db
vault: /tmp/legacy-vault
editor: nano
EOF

    run $BREX brain list
    [ "$status" -eq 0 ]
    [[ "$output" =~ "legacy-vault -> /tmp/legacy.db" ]]
    [[ ! "$output" =~ "(none)" ]]
}

@test "error on invalid brain name" {
    run $BREX invalid_brain task list
    [ "$status" -eq 1 ]
    [[ "$output" =~ "Error: Brain 'invalid_brain' not configured" ]]
}
