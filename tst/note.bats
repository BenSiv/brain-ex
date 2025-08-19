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
