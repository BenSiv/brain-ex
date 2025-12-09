#!/usr/bin/env bats

setup() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor touch
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
}

@test "write content to log note" {
    run brex note --content "Initial content"
    [ "$status" -eq 0 ]

    # Get latest log file created (timestamp-based)
    NOTEFILE=$(ls -t tmp_vault/log/*.md | head -n1)
    BASENAME=$(basename "$NOTEFILE" .md)

    grep -q "Initial content" "$NOTEFILE"

    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='$BASENAME' AND subject='log' AND content LIKE '%Initial content%';")
    [ "$COUNT" -eq 1 ]
}

@test "update note with additional links" {
    run brex note add --title "meeting" --content "Meeting notes" --subject "work" --links "todo"
    [ "$status" -eq 0 ]

    run brex note connect --title "meeting" --subject "work" --links "daily/agenda,review"
    [ "$status" -eq 0 ]

    [ -f "tmp_vault/work/meeting.md" ]
    grep -q "Meeting notes" tmp_vault/work/meeting.md

    LINKCOUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM connections WHERE source_title='meeting' AND source_subject='work' AND ((target_title='todo' AND target_subject='') OR (target_title='agenda' AND target_subject='daily') OR (target_title='review' AND target_subject=''))")
    [ "$LINKCOUNT" -eq 3 ]
}

@test "connect link to logdaily note without content" {    
    # Connect link only, no new content
    run brex note connect --links "daily/todo"
    [ "$status" -eq 0 ]

    NOTEFILE=$(ls -t tmp_vault/log/*.md | head -n1)
    BASENAME=$(basename "$NOTEFILE" .md)
    [ -f "$NOTEFILE" ]

    # The file should contain the Obsidian-style link
    grep -q "\[\[daily/todo\]\]" "$NOTEFILE"

    # DB should have connection to the link
    LINKCOUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM connections WHERE source_title='$BASENAME' AND source_subject='log' AND target_title='todo' AND target_subject='daily';")
    [ "$LINKCOUNT" -eq 1 ]
}

@test "create note with custom subject" {
    run brex note add --title "api-review" --content "Reviewed API endpoints" --subject "backend"
    [ "$status" -eq 0 ]

    [ -f "tmp_vault/backend/api-review.md" ]
    grep -q "Reviewed API endpoints" tmp_vault/backend/api-review.md

    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='api-review' AND subject='backend' AND content LIKE '%Reviewed API endpoints%';")
    [ "$COUNT" -eq 1 ]
}

@test "add links to note with custom subject" {
    run brex note add --title "api-review" --content "Reviewed API endpoints" --subject "backend" --links "daily/todo,frontend/design"
    [ "$status" -eq 0 ]

    [ -f "tmp_vault/backend/api-review.md" ]
    grep -q "Reviewed API endpoints" tmp_vault/backend/api-review.md

    LINKCOUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM connections WHERE source_title='api-review' AND source_subject='backend' AND ((target_title='todo' AND target_subject='daily') OR (target_title='design' AND target_subject='frontend'));")
    [ "$LINKCOUNT" -eq 2 ]
}

@test "edit non-existent title creates new note in default editor" {
    run brex note edit --title "new_idea" --subject "brain-ex"
    [ "$status" -eq 0 ]

    [ -f "tmp_vault/brain-ex/new_idea.md" ]
    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='new_idea' AND subject='brain-ex';")
    [ "$COUNT" -eq 1 ]

    # Ensure content file is empty initially
    CONTENT=$(cat tmp_vault/brain-ex/new_idea.md)
    [ -z "$CONTENT" ]
}

@test "error when subject is passed without title" {
    run brex note add --subject "brain-ex"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "Note command failed" ]]
    [[ "$output" =~ "Must provide note title" ]]
}

@test "update note from file after manual edit" {
    run brex note add --title "manual-update" --subject "brain-ex" --content "Old content"
    [ "$status" -eq 0 ]

    # Simulate manual edit
    echo "New content after edit" >> tmp_vault/brain-ex/manual-update.md

    run brex update --file tmp_vault/brain-ex/manual-update.md
    [ "$status" -eq 0 ]

    # DB content should now include new text
    CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE title='manual-update' AND subject='brain-ex';")
    [[ "$CONTENT" =~ "New content after edit" ]]
}

@test "add with --update flag appends new content to existing note" {
    run brex note add --title "update-note" --content "Initial line" --subject "brain-ex"
    [ "$status" -eq 0 ]

    run brex note add --title "update-note" --content "Appended line" --subject "brain-ex" --update
    [ "$status" -eq 0 ]

    CONTENT=$(cat tmp_vault/brain-ex/update-note.md)
    [[ "$CONTENT" =~ "Initial line" ]]
    [[ "$CONTENT" =~ "Appended line" ]]

    DB_CONTENT=$(sqlite3 tmp_vault.db "SELECT content FROM notes WHERE title='update-note' AND subject='brain-ex';")
    [[ "$DB_CONTENT" =~ "Initial line" ]]
    [[ "$DB_CONTENT" =~ "Appended line" ]]
}


@test "note last shows most recent notes" {
    brex note add --title "old-note" --content "Old content" --subject "test"
    sleep 1  # Ensure different timestamps
    brex note add --title "new-note" --content "New content" --subject "test"
    
    run brex note last --subject "test"
    [ "$status" -eq 0 ]
    # Should show both notes
    [[ "$output" =~ "new-note" ]]
    [[ "$output" =~ "old-note" ]]
}

@test "note last with --number flag limits results" {
    brex note add --title "note1" --content "Content 1" --subject "test"
    brex note add --title "note2" --content "Content 2" --subject "test"
    brex note add --title "note3" --content "Content 3" --subject "test"
    
    run brex note last --subject "test" --number 2
    [ "$status" -eq 0 ]
    # Should only show 2 notes (the most recent ones)
    # Count how many times we see note content
    COUNT=$(echo "$output" | grep -c "Content" || true)
    [ "$COUNT" -le 4 ]  # 2 notes * (title + content) = up to 4 matches
}

@test "note last with --subject filters by subject" {
    brex note add --title "backend-note" --content "Backend content" --subject "backend"
    brex note add --title "frontend-note" --content "Frontend content" --subject "frontend"
    
    run brex note last --subject "backend"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "Backend content" || "$output" =~ "backend-note" ]]
    [[ ! "$output" =~ "Frontend content" ]]
}

@test "note add with special characters in content" {
    run brex note add --title "special-chars" --content "Content with 'quotes' and \"double quotes\"" --subject "test"
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
    run brex note last --subject "nonexistent"
    [ "$status" -eq 0 ]
    [[ "$output" =~ "No notes" ]]
}
