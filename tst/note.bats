#!/usr/bin/env bats

setup() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
    mkdir tmp_vault
    brex init --vault tmp_vault --editor micro
}

teardown() {
    rm -rf tmp_vault
    rm -f tmp_vault.db
}

@test "append content to existing daily note" {
    run brex note --content "Initial content"
    [ "$status" -eq 0 ]

    TODAY=$(date +%Y-%m-%d)
    NOTEFILE="tmp_vault/daily/$TODAY.md"

    run brex note --content "Appended content"
    [ "$status" -eq 0 ]

    grep -q "Initial content" "$NOTEFILE"
    grep -q "Appended content" "$NOTEFILE"

    COUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM notes WHERE title='$TODAY' AND subject='daily' AND content LIKE '%Initial content%Appended content%';")
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

@test "connect link to daily note without content" {
    TODAY=$(date +%Y-%m-%d)
    
    # Connect link only, no new content
    run brex note connect --links "daily/todo"
    [ "$status" -eq 0 ]

    NOTEFILE="tmp_vault/daily/$TODAY.md"
    [ -f "$NOTEFILE" ]

    # The file should contain the Obsidian-style link
    grep -q "\[\[daily/todo\]\]" "$NOTEFILE"

    # DB should have connection to the link
    LINKCOUNT=$(sqlite3 tmp_vault.db "SELECT COUNT(*) FROM connections WHERE source_title='$TODAY' AND source_subject='daily' AND target_title='todo' AND target_subject='daily';")
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
    [[ "$output" =~ "Error" ]]
    [[ "$output" =~ "title is required" ]]
}

@test "update note from file after manual edit" {
    run brex note add --title "manual-update" --subject "brain-ex" --content "Old content"
    [ "$status" -eq 0 ]

    # Simulate manual edit
    echo "New content after edit" >> tmp_vault/brain-ex/manual-update.md

    run brex note update --title "manual-update" --subject "brain-ex" --from-file
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

