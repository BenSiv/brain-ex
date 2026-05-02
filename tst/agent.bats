#!/usr/bin/env bats

load test_helper.bash
BREX="brex"

resolve_brex() {
    if [ -x "$PROJECT_ROOT/bin/brex" ]; then
        BREX="$PROJECT_ROOT/bin/brex"
    else
        BREX="brex"
    fi
}

setup() {
    setup_test_env
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
    cleanup_test_env
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

@test "agent defaults to ask when prompt is provided without subcommand" {
    export BREX_MOCK_RESPONSE="<done>hello from default ask</done>"
    run $BREX mybrain agent "hello world"
    [ "$status" -eq 0 ]
    [[ "$output" == *"hello from default ask"* ]]
}

@test "agent joins multiple words as a single prompt" {
    export BREX_MOCK_RESPONSE="<done>joined prompt works</done>"
    run $BREX mybrain agent ask hello multiple words
    [ "$status" -eq 0 ]
    [[ "$output" == *"joined prompt works"* ]]
    [[ "$output" != *"Warning: subagent"* ]]
    # Also test the default path
    run $BREX mybrain agent hello multiple words
    [ "$status" -eq 0 ]
    [[ "$output" == *"joined prompt works"* ]]
    [[ "$output" != *"Warning: subagent"* ]]
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

@test "agent can retrieve note data using note.read" {
    # Pre-create a note
    mkdir -p tmp_vault/research
    echo "Existing research content" > tmp_vault/research/study.md
    $BREX mybrain update --file tmp_vault/research/study.md

    export BREX_MOCK_RESPONSE_1=$'<tool>note</tool>\n<method>read</method>\n<args>subject=research\ntitle=study</args>'
    export BREX_MOCK_RESPONSE_2="<done>I found the note</done>"
    
    run $BREX mybrain agent ask "What is in the study note?"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Agent requested tool: note.read"* ]]
    [[ "$output" == *"I found the note"* ]]
}

@test "agent can retrieve tasks using task.list" {
    $BREX mybrain task add --content "Test task 1" --subject "test"
    
    export BREX_MOCK_RESPONSE_1=$'<tool>task</tool>\n<method>list</method>\n<args></args>'
    export BREX_MOCK_RESPONSE_2="<done>I see your tasks</done>"
    
    run $BREX mybrain agent ask "What tasks do I have?"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Agent requested tool: task.list"* ]]
    [[ "$output" == *"I see your tasks"* ]]
}

@test "agent can execute raw sql queries" {
    $BREX mybrain note --content "SQL test note"
    
    export BREX_MOCK_RESPONSE_1=$'<tool>sql</tool>\n<method>query</method>\n<args>query=SELECT content FROM notes WHERE content LIKE \'%SQL test%\';</args>'
    export BREX_MOCK_RESPONSE_2="<done>I checked the database</done>"
    
    run $BREX mybrain agent ask "Find notes about SQL test"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Agent requested tool: sql.query"* ]]
    [[ "$output" == *"I checked the database"* ]]
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
