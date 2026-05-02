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
    cleanup_test_env
}

@test "knowledge search returns ranked pool results" {
    run $BREX knowledge_brain knowledge search "neural"
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "retrieval_id" ]]
    [[ "$output" =~ "Deep Learning" ]]
}

@test "knowledge search handles no results" {
    run $BREX knowledge_brain knowledge search "nonexistent"
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ ! "$output" =~ "Deep Learning" ]]
}

@test "knowledge search requires a query" {
    run $BREX knowledge_brain knowledge search
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Usage: brex knowledge search" ]]
}

@test "knowledge search honors limit" {
    run $BREX knowledge_brain knowledge search "is" --limit 1
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "retrieval_id" ]]
    row_count="$(printf "%s\n" "$output" | grep -E "Deep Learning|SQL Basics" | wc -l)"
    [ "$row_count" -eq 1 ]
}

@test "knowledge sync runs without error" {
    run $BREX knowledge_brain knowledge sync
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Synced" ]]
    [ "$(sqlite3 knowledge_brain.db "SELECT COUNT(*) FROM knowledge_items;")" -eq 2 ]
    [ "$(sqlite3 knowledge_brain.db "SELECT COUNT(*) FROM knowledge_retrievals;")" -eq 0 ]
}

@test "knowledge schema initializes all control-plane tables" {
    run $BREX knowledge_brain sql --query "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE 'knowledge_%' ORDER BY name;"
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "knowledge_items" ]]
    [[ "$output" =~ "knowledge_retrievals" ]]
    [[ "$output" =~ "knowledge_retrieval_results" ]]
    [[ "$output" =~ "knowledge_links" ]]
    [[ "$output" =~ "knowledge_reviews" ]]
}

@test "knowledge browse shows synced provenance metadata" {
    run $BREX knowledge_brain knowledge browse
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "SQL Basics" ]]
    [[ "$output" =~ "data/SQL Basics.md" ]]
}

@test "knowledge browse honors limit" {
    run $BREX knowledge_brain knowledge browse --limit 1
    echo "Output: $output"
    [ "$status" -eq 0 ]
    row_count="$(printf "%s\n" "$output" | grep -E "Deep Learning|SQL Basics" | wc -l)"
    [ "$row_count" -eq 1 ]
}

@test "knowledge show displays one item content and metadata" {
    run $BREX knowledge_brain knowledge show 1
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Deep Learning" ]]
    [[ "$output" =~ "Artificial neural networks" ]]
    [[ "$output" =~ "ai/Deep Learning.md" ]]
}

@test "knowledge show on unknown item returns empty table" {
    run $BREX knowledge_brain knowledge show 999
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Empty table" ]]
}

@test "knowledge show requires an id" {
    run $BREX knowledge_brain knowledge show
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Usage: brex knowledge show" ]]
}

@test "knowledge history records retrieval queries" {
    run $BREX knowledge_brain knowledge search "neural"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge history
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "neural" ]]
}

@test "knowledge history for an item shows rank and score" {
    run $BREX knowledge_brain knowledge search "neural"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge history 1
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "neural" ]]
    [[ "$output" =~ "rank" ]]
    [[ "$output" =~ "score" ]]
}

@test "knowledge retrieval stores result audit and co-retrieval links" {
    run $BREX knowledge_brain knowledge search "neural database" --limit 2
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [ "$(sqlite3 knowledge_brain.db "SELECT COUNT(*) FROM knowledge_retrieval_results;")" -ge 2 ]
    [ "$(sqlite3 knowledge_brain.db "SELECT COUNT(*) FROM knowledge_links WHERE link_type='co-retrieved';")" -ge 1 ]
}

@test "knowledge process detects exact duplicates" {
    $BREX knowledge_brain note add --title "Neural Copy" --content "Artificial neural networks are inspired by biological brains." --subject "ai"

    run $BREX knowledge_brain knowledge process
    echo "Output: $output"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge queue
    echo "Queue: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "duplicate" ]]
    [ "$(sqlite3 knowledge_brain.db "SELECT COUNT(*) FROM knowledge_reviews WHERE duplication_status='duplicate';")" -ge 1 ]
}

@test "knowledge queue promotes repeated retrieval candidates" {
    run $BREX knowledge_brain knowledge search "neural"
    [ "$status" -eq 0 ]
    run $BREX knowledge_brain knowledge search "neural"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge queue
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Deep Learning" ]]
    [[ "$output" =~ "ready" ]]
}

@test "knowledge promote materializes a vault artifact" {
    run $BREX knowledge_brain knowledge promote 1 --tier 2
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Promoted 1" ]]
    [ -f tmp_vault/knowledge/tier2/deep-learning-1.md ]
    grep -q "Source: ai/Deep Learning.md" tmp_vault/knowledge/tier2/deep-learning-1.md
}

@test "knowledge promote tier 3 materializes atomic artifact state" {
    run $BREX knowledge_brain knowledge promote 1 --tier 3
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [ -f tmp_vault/knowledge/tier3/deep-learning-1.md ]
    grep -q "Tier: 3" tmp_vault/knowledge/tier3/deep-learning-1.md
    grep -q "Process: atomic" tmp_vault/knowledge/tier3/deep-learning-1.md
    [ "$(sqlite3 knowledge_brain.db "SELECT artifact_status FROM knowledge_items WHERE id=1;")" = "materialized" ]
}

@test "knowledge promote refuses duplicate items" {
    $BREX knowledge_brain note add --title "Neural Copy" --content "Artificial neural networks are inspired by biological brains." --subject "ai"
    $BREX knowledge_brain knowledge process
    duplicate_id="$(sqlite3 knowledge_brain.db "SELECT id FROM knowledge_items WHERE duplicate_of IS NOT NULL LIMIT 1;")"

    run $BREX knowledge_brain knowledge promote "$duplicate_id" --tier 2
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Refusing to promote duplicate item" ]]
}

@test "knowledge promote requires a valid item id" {
    run $BREX knowledge_brain knowledge promote
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Must provide a knowledge item id" ]]

    run $BREX knowledge_brain knowledge promote 999
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Knowledge item not found" ]]
}

@test "knowledge promote requires a vault-backed brain" {
    rm -f bare_brain.db
    run $BREX init --name bare_brain --editor touch
    [ "$status" -eq 0 ]
    sed -i '/^vault:/d' "$CONFIG"
    $BREX bare_brain note add --title "Standalone" --content "No vault exists here." --subject "test"

    run $BREX bare_brain knowledge promote 1 --tier 2
    echo "Output: $output"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "No vault configured" ]]
    rm -f bare_brain.db
}

@test "knowledge process marks promoted artifacts stale after source changes" {
    run $BREX knowledge_brain knowledge promote 1 --tier 2
    [ "$status" -eq 0 ]

    $BREX knowledge_brain note add --title "Deep Learning" --subject "ai" --content "Added source detail." --update
    run $BREX knowledge_brain knowledge process
    echo "Process: $output"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge queue
    echo "Queue: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "stale" ]]
    [ "$(sqlite3 knowledge_brain.db "SELECT artifact_status FROM knowledge_items WHERE id=1;")" = "stale" ]
}

@test "knowledge update --file resyncs changed note metadata" {
    printf "Updated SQL Basics content mentions joins and indexes.\n" > "tmp_vault/data/SQL Basics.md"

    run $BREX knowledge_brain update --file "tmp_vault/data/SQL Basics.md"
    echo "Update: $output"
    [ "$status" -eq 0 ]

    run $BREX knowledge_brain knowledge search "indexes"
    echo "Search: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "SQL Basics" ]]
    [ "$(sqlite3 knowledge_brain.db "SELECT content FROM knowledge_items WHERE source_id='data/SQL Basics';")" = "Updated SQL Basics content mentions joins and indexes." ]
}

@test "knowledge unknown subcommand shows usage" {
    run $BREX knowledge_brain knowledge unknown
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Usage: brex knowledge" ]]
}

@test "knowledge command without subcommand shows usage" {
    run $BREX knowledge_brain knowledge
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Usage: brex knowledge" ]]
}
