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
    rm -f tmp_vault.db
    mkdir tmp_vault
    $BREX init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    cleanup_test_env
}

@test "update with --file flag updates existing note" {
    # Create a note
    $BREX note add --title "update-test" --content "Original content" --subject "test"
    
    # Manually edit the file
    echo "Modified content in file" > tmp_vault/test/update-test.md
    
    # Update from file
    run $BREX update --file tmp_vault/test/update-test.md
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Updated note" ]]
    
    # Check database was updated
    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE title='update-test' AND subject='test';")
    [[ "$CONTENT" =~ "Modified content in file" ]]
}

@test "update with --file on non-existent note creates it" {
    # Create a file without database entry
    mkdir -p tmp_vault/newsubject
    echo "New note content" > tmp_vault/newsubject/newnote.md
    
    # Update from file (should create in DB)
    run $BREX update --file tmp_vault/newsubject/newnote.md
    [ "$status" -eq 0 ]
    
    # Check database has the note
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='newnote' AND subject='newsubject';")
    [ "$COUNT" -eq 1 ]
}

@test "update with --file on missing file shows error" {
    run $BREX update --file tmp_vault/nonexistent/missing.md
    [ "$status" -ne 0 ]
    [[ "$output" =~ "does not exist" ]] || [[ "$output" =~ "Failed" ]]
}

@test "update without --file rebuilds database from vault notes and tasks" {
    mkdir -p tmp_vault/work
    echo "Root note content" > tmp_vault/root_note.md
    printf "Work note content\n[[root_note]]\n" > tmp_vault/work/rebuild.md

    # Insert DB-only record that should disappear after full rebuild
    sqlite3 tmp_vault.db "INSERT INTO notes(subject, title, content) VALUES('tmp', 'db_only', 'should be removed');"

    # Prepare task markdown file to verify task import after rebuild --
    # a task's file lives at <subject>/<title>.md, same as any note.
    mkdir -p tmp_vault/ops
    cat <<EOF > "tmp_vault/ops/Imported Task.md"
---
id: 101
title: "Imported Task"
is_task: true
time: "2026-01-01 10:00:00"
due_to: "2026-01-02 10:00:00"
overdue: 0
importance: 1
urgency: 1
---
Imported Task body
EOF

    run $BREX update
    [ "$status" -eq 0 ]

    ROOT_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='root_note' AND subject='';")
    [ "$ROOT_COUNT" -eq 1 ]

    WORK_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='rebuild' AND subject='work';")
    [ "$WORK_COUNT" -eq 1 ]

    STALE_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='db_only' AND subject='tmp';")
    [ "$STALE_COUNT" -eq 0 ]

    TASK_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes JOIN tasks ON tasks.item_id = notes.id WHERE notes.title='Imported Task' AND notes.subject='ops';")
    [ "$TASK_COUNT" -eq 1 ]
}

@test "update with invalid file path shows error" {
    # Create a directory instead of a file
    mkdir -p tmp_vault/test/notafile
    
    run $BREX update --file tmp_vault/test/notafile
    [ "$status" -ne 0 ]
}

@test "update from file in root directory with no subject" {
    echo "Content in root" > tmp_vault/root_note.md
    
    run $BREX update --file tmp_vault/root_note.md
    [ "$status" -eq 0 ]
    
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='root_note' AND subject='';")
    [ "$COUNT" -eq 1 ]
}

@test "migrate legacy agent_sessions.tsv, agent_messages.tsv to Markdown" {
    # tasks.tsv migration was removed along with the flat tasks table
    # (see doc/unified-items-design.md) -- its column shape no longer
    # maps onto anything a current brain has.

    # 1. Create legacy TSV files
    cat <<EOF > tmp_vault/agent_sessions.tsv
id	name	created_at	updated_at
session-123	Test Session	2026-06-05 10:00:00	2026-06-05 10:00:00
EOF

    cat <<EOF > tmp_vault/agent_messages.tsv
id	session_id	role	content	metadata
1	session-123	user	Hello	NULL
EOF

    # 2. Run update to trigger migration
    run $BREX update
    [ "$status" -eq 0 ]

    # 3. Verify TSVs are deleted
    [ ! -f tmp_vault/agent_sessions.tsv ]
    [ ! -f tmp_vault/agent_messages.tsv ]

    # 4. Verify Markdown files are generated
    [ -f tmp_vault/agent_sessions/session-123.md ]

    # 5. Verify data is preserved in DB
    SESSION_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM agent_sessions WHERE id='session-123';")
    [ "$SESSION_COUNT" -eq 1 ]
    MSG_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM agent_messages WHERE session_id='session-123' AND content='Hello';")
    [ "$MSG_COUNT" -eq 1 ]
}
