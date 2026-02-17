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
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    $BREX init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    rm -f "$CONFIG"
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

    # Prepare tasks TSV to verify task import after rebuild
    printf "id\ttime\tcontent\tsubject\tdue_to\toverdue\tdone\tcomment\n101\t2026-01-01 10:00:00\tImported Task\tops\t2026-01-02 10:00:00\t0\t\t\n" > tmp_vault/tasks.tsv

    run $BREX update
    [ "$status" -eq 0 ]

    ROOT_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='root_note' AND subject='';")
    [ "$ROOT_COUNT" -eq 1 ]

    WORK_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='rebuild' AND subject='work';")
    [ "$WORK_COUNT" -eq 1 ]

    STALE_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='db_only' AND subject='tmp';")
    [ "$STALE_COUNT" -eq 0 ]

    TASK_COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM tasks WHERE id=101 AND content='Imported Task';")
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
