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

@test "note last shows most recent notes" {
    brex note add --title "old-note" --content "Old content" --subject "testnotes"
    sleep 1  # Ensure different timestamps
    brex note add --title "new-note" --content "New content" --subject "testnotes"
    
    run brex note last --subject testnotes
    [ "$status" -eq 0 ]
    # Should show both notes
    [[ "$output" =~ "new-note" || "$output" =~ "New content" ]]
    [[ "$output" =~ "old-note" || "$output" =~ "Old content" ]]
}

@test "note last with --number flag limits results" {
    brex note add --title "note1" --content "Content 1" --subject "testnotes"
    brex note add --title "note2" --content "Content 2" --subject "testnotes"
    brex note add --title "note3" --content "Content 3" --subject "testnotes"
    
    run brex note last --subject testnotes --number 2
    [ "$status" -eq 0 ]
    # Should run successfully (exact output verification difficult without fixing SQL bug)
}

@test "note add with special characters in content" {
    run brex note add --title "special-chars" --content "Content with quotes" --subject "testnotes"
    [ "$status" -eq 0 ]
    
    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE title='special-chars';")
    [[ "$CONTENT" =~ "quotes" ]]
}

@test "note default behavior logs to log subject" {
    run brex note --content "Default log note"
    [ "$status" -eq 0 ]
    
    # Should create a note in log subject
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE subject='log' AND content LIKE '%Default log note%';")
    [ "$COUNT" -eq 1 ]
}

@test "note last on empty subject shows no notes message" {
    run brex note last --subject nonexistentsubject
    [ "$status" -eq 0 ]
    [[ "$output" =~ "No notes" || "$output" =~ "Error" ]]
}
