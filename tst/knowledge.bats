#!/usr/bin/env bats

# Use a temporary HOME to avoid messing with the user's config
export HOME="$BATS_TMPDIR"
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
    rm -rf tmp_vault
    rm -f knowledge_brain.db
    mkdir -p tmp_vault
    $BREX init --name knowledge_brain --vault tmp_vault --editor touch

    # Add some sample notes
    $BREX knowledge_brain note add --title "Deep Learning" --content "Artificial neural networks are inspired by biological brains." --subject "ai"
    $BREX knowledge_brain note add --title "SQL Basics" --content "Structured Query Language is used for managing databases." --subject "data"
}

teardown() {
    rm -rf tmp_vault
    rm -f knowledge_brain.db
    rm -f "$CONFIG"
}

@test "knowledge search works with LIKE fallback" {
    run $BREX knowledge_brain knowledge search "neural"
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Deep Learning" ]]
}

@test "knowledge search handles no results" {
    run $BREX knowledge_brain knowledge search "nonexistent"
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ ! "$output" =~ "Deep Learning" ]]
}

@test "knowledge sync runs without error" {
    run $BREX knowledge_brain knowledge sync
    echo "Output: $output"
    [ "$status" -eq 0 ]
}

@test "knowledge command without subcommand shows usage" {
    run $BREX knowledge_brain knowledge
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Usage: brex knowledge" ]]
}
