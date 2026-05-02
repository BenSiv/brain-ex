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
    mkdir -p "$HOME"
    rm -rf tmp_vault
    rm -f mybrain.db
    mkdir -p tmp_vault
    $BREX init --name mybrain --vault tmp_vault --editor touch
    {
        echo 'agent_provider: mock'
        echo 'agent_model: test-model'
    } >> "$CONFIG"
}

teardown() {
    rm -rf tmp_vault
    rm -f mybrain.db
    rm -f "$CONFIG"
}

@test "agent command defaults to view" {
    # Set PAGER=cat to avoid blocking in tests
    export PAGER=cat
    touch agent.log
    run $BREX mybrain agent
    [ "$status" -eq 0 ]
}

@test "agent view works" {
    export PAGER=cat
    touch agent.log
    run $BREX mybrain agent view
    [ "$status" -eq 0 ]
}

@test "agent help flag works" {
    run $BREX mybrain agent -h
    [ "$status" -eq 0 ]
    [[ "$output" == *"Manage and interact with the Brain-Ex AI agent"* ]]
    [[ "$output" == *"ask <prompt>"* ]]
    [[ "$output" == *"note <prompt>"* ]]
    [[ "$output" == *"task <prompt>"* ]]
    [[ "$output" != *"agent tasks"* ]]
    [[ "$output" != *"agent run"* ]]
}

@test "agent ask works" {
    export BREX_MOCK_RESPONSE="<done>hello from ask</done>"
    run $BREX mybrain agent ask "hello"
    [ "$status" -eq 0 ]
    [[ "$output" == *"hello from ask"* ]]
}

@test "agent note can write through the vault sync path" {
    export BREX_MOCK_RESPONSE_1=$'<tool>note</tool>\n<method>add</method>\n<args>subject=work\ntitle=agent-note\ncontent=Created by agent</args>'
    export BREX_MOCK_RESPONSE_2="<done>note saved</done>"
    run $BREX mybrain agent note "capture this"
    [ "$status" -eq 0 ]
    [ -f "tmp_vault/work/agent-note.md" ]
    grep -q "Created by agent" tmp_vault/work/agent-note.md
    COUNT=$(sqlite3 mybrain.db "SELECT COUNT(*) FROM notes WHERE title='agent-note' AND subject='work';")
    [ "$COUNT" -eq 1 ]
}

@test "agent task can write through the tasks sync path" {
    export BREX_MOCK_RESPONSE_1=$'<tool>task</tool>\n<method>add</method>\n<args>subject=ops\ncontent=Agent task\ndue_to=2026-05-02</args>'
    export BREX_MOCK_RESPONSE_2="<done>task saved</done>"
    run $BREX mybrain agent task "create a task"
    [ "$status" -eq 0 ]
    grep -q "Agent task" tmp_vault/tasks.tsv
    COUNT=$(sqlite3 mybrain.db "SELECT COUNT(*) FROM tasks WHERE content='Agent task' AND subject='ops' AND owner='agent';")
    [ "$COUNT" -eq 1 ]
}

@test "agent tasks subcommand is rejected with task list guidance" {
    run $BREX mybrain agent tasks
    [ "$status" -ne 0 ]
    [[ "$output" == *"brex task list --owner agent"* ]]
}

@test "agent run subcommand is rejected with explicit alternatives" {
    run $BREX mybrain agent run "hello"
    [ "$status" -ne 0 ]
    [[ "$output" == *"brex agent ask"* ]]
}
