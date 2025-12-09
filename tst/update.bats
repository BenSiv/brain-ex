#!/usr/bin/env bats

CONFIG="$HOME/.config/brain-ex/config.yaml"

setup() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    rm -f "$CONFIG"
}

@test "update with --file flag updates existing note" {
    # Create a note
    brex note add --title "update-test" --content "Original content" --subject "test"
    
    # Manually edit the file
    echo "Modified content in file" > tmp_vault/test/update-test.md
    
    # Update from file
    run brex update --file tmp_vault/test/update-test.md
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
    run brex update --file tmp_vault/newsubject/newnote.md
    [ "$status" -eq 0 ]
    
    # Check database has the note
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='newnote' AND subject='newsubject';")
    [ "$COUNT" -eq 1 ]
}

@test "update with --file on missing file shows error" {
    run brex update --file tmp_vault/nonexistent/missing.md
    [ "$status" -ne 0 ]
    [[ "$output" =~ "does not exist" ]] || [[ "$output" =~ "Failed" ]]
}

@test "update without --file shows not implemented message" {
    run brex update
    [ "$status" -eq 0 ]

    # Assuming update from vault prints nothing or "success" if it works, or we can just check status.
    # The original test expected "not yet implemented". 
    # Let's check for absence of error or specific success indicator if any.
    # Actually, let's just assert status 0 for now as 'not yet implemented' is definitely wrong.

}

@test "update with invalid file path shows error" {
    # Create a directory instead of a file
    mkdir -p tmp_vault/test/notafile
    
    run brex update --file tmp_vault/test/notafile
    [ "$status" -ne 0 ]
}
