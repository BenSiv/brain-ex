#!/usr/bin/env bats

load test_helper.bash

setup() {
    setup_test_env
    export TEST_DIR="$PROJECT_ROOT/.tmp-daat-sync-$$"
    mkdir -p "$TEST_DIR/brex" "$TEST_DIR/daat"

    export DAAT_BIN="${DAAT_BIN:-$(cd "$PROJECT_ROOT/../daat" 2>/dev/null && pwd)/bin/daat}"
    if [ ! -x "$DAAT_BIN" ] && command -v daat >/dev/null 2>&1; then
        DAAT_BIN="$(command -v daat)"
    fi
    if [ ! -x "$DAAT_BIN" ]; then
        skip "daat binary not found"
    fi

    # Initialize clean SQLite-only brex brain (no Obsidian vault).
    (
        cd "$TEST_DIR/brex"
        brex init --name notes_test --editor cat
        brex brain use --name notes_test
    )

    # Set up fresh daat store with the bensiv-notes task extension & prioritized_tasks view.
    mkdir -p "$TEST_DIR/daat/extensions/task/schemas" "$TEST_DIR/daat/extensions/task/views"
    cat > "$TEST_DIR/daat/extensions/task/manifest.lua" <<'EOF'
return {
    name = "task",
    events = {"entity.before_create", "entity.before_update"},
    entity_types = {"task"},
    capabilities = {
        read = {"entity"},
        write = {"entity"},
        net = "none",
        schemas = {"task"},
        views = {"prioritized_tasks"},
    },
}
EOF
    cat > "$TEST_DIR/daat/extensions/task/schemas/task.lua" <<'EOF'
return {
    name = "task",
    fields = {
        {name = "document", type = "reference", required = true, label = "Task", entity_type = "document", display = true},
        {name = "subject", type = "text", required = false, label = "Subject"},
        {name = "due_to", type = "date", required = false, label = "Due"},
        {name = "owner", type = "text", required = false, label = "Owner"},
        {name = "importance", type = "number", required = true, label = "Importance", min = 1, max = 5},
        {name = "urgency", type = "number", required = true, label = "Urgency", min = 1, max = 5},
        {name = "status", type = "select", required = true, label = "Status", values = {"open", "done"}},
    },
}
EOF
    cat > "$TEST_DIR/daat/extensions/task/views/prioritized_tasks.lua" <<'EOF'
return {
    name = "prioritized_tasks",
    title = "Prioritized tasks",
    entity_type = "task",
    sql = "SELECT * FROM (SELECT t.id, d.title AS title, t.subject, t.due_to, t.owner, t.importance, t.urgency, t.status, t.urgency AS active_urgency FROM task t LEFT JOIN document d ON d.id = t.document WHERE t.status = 'open' AND t.archived_at IS NULL) ORDER BY importance DESC;",
    columns = {
        {name = "id", label = "ID"},
        {name = "title", label = "Task"},
        {name = "subject", label = "Subject"},
        {name = "status", label = "Status"},
    },
}
EOF
    cat > "$TEST_DIR/daat/extensions/task/main.lua" <<'EOF'
function is_active(row)
    return row.archived_at == nil or row.archived_at == ""
end
function on_before(new, old, ctx)
    doc_id = new.document
    if doc_id != nil and tostring(doc_id) != "" then
        if old == nil or tostring(old.document) != tostring(doc_id) then
            for _, other in ipairs(ctx.query("task", {document = doc_id})) do
                if is_active(other) and (old == nil or tostring(other.id) != tostring(old.id)) then
                    return {{field = "document", severity = "error", message = "already has task"}}
                end
            end
        end
    end
    return {}
end
return {on_before = on_before}
EOF

    (
        cd "$TEST_DIR/daat"
        "$DAAT_BIN" init >/dev/null
        "$DAAT_BIN" extension approve task >/dev/null
        "$DAAT_BIN" schema sync >/dev/null
        "$DAAT_BIN" view approve prioritized_tasks >/dev/null
    )
    RAW_KEY=$(cd "$TEST_DIR/daat" && "$DAAT_BIN" api-key create brex-sync ia | grep -Eo '[A-Za-z0-9_-]{24,}' | tail -n 1)
    echo "$RAW_KEY" > "$TEST_DIR/daat-api-key"
    chmod 600 "$TEST_DIR/daat-api-key"

    cat >> "$CONFIG" <<EOF
daat_url: http://daat.local
daat_api_key_file: $TEST_DIR/daat-api-key
EOF

    export BREX_DAAT_CGI_BIN="$DAAT_BIN"
    export BREX_DAAT_CGI_DIR="$TEST_DIR/daat"
    export BREX_DB="$TEST_DIR/brex/notes_test.db"
    export DAAT_DB="$TEST_DIR/daat/.store/store.db"
}

teardown() {
    rm -rf "$TEST_DIR"
    cleanup_test_env
}

daat_api_write() {
    local method="$1"
    local path="$2"
    local body="$3"
    local key
    key="$(cat "$TEST_DIR/daat-api-key")"
    (
        cd "$TEST_DIR/daat"
        printf '%s' "$body" | GATEWAY_INTERFACE="CGI/1.1" REQUEST_METHOD="$method" \
            PATH_INFO="$path" QUERY_STRING="" HTTP_X_API_KEY="$key" \
            CONTENT_TYPE="application/json" "$DAAT_BIN"
    ) | sed '1,/^\r\{0,1\}$/d'
}

@test "DB-only brex note edit works without an Obsidian vault and extracts wikilinks" {
    cat > "$TEST_DIR/fake-editor.sh" <<'EOF'
#!/usr/bin/env bash
printf 'Edited body linking to [[daat/Target Note]] and quotes '\''ok'\''.\n' > "$1"
EOF
    chmod +x "$TEST_DIR/fake-editor.sh"
    sed -i "s|editor: cat|editor: $TEST_DIR/fake-editor.sh|" "$CONFIG"

    run brex note edit -s daat -t "Vaultless Edit"
    [ "$status" -eq 0 ]

    CONTENT=$(sqlite3 "$BREX_DB" "SELECT content FROM notes WHERE subject='daat' AND title='Vaultless Edit';")
    [[ "$CONTENT" == *"Edited body linking to [[daat/Target Note]]"* ]]

    CONN=$(sqlite3 "$BREX_DB" "SELECT target_subject || '/' || target_title FROM connections WHERE source_title='Vaultless Edit';")
    [ "$CONN" = "daat/Target Note" ]
}

@test "initial batch sync pushes notes, subject folders, wikilinks, and tasks to daat, and steady-state sync is a no-op" {
    brex note add -s daat -t "Architecture" -c "Core overview."
    brex note add -s daat -t "Sync Design" -c "Builds on core." -l "daat/Architecture"
    brex note add -t "Root Scratch" -c "Top-level thought."
    brex task add -s benchling -t "Fix sample lineage" -c "Investigate SCM5C1." -e "2026-12-01" -p 5 -u 4 -o ben

    run brex sync --dry-run
    [ "$status" -eq 0 ]
    [[ "$output" == *"Dry-run sync with daat: pushed 4 doc(s)/1 task(s)"* ]]

    run brex sync
    [ "$status" -eq 0 ]
    [[ "$output" == *"pushed 4 doc(s)/1 task(s)"* ]]
    [[ "$output" == *"folders_created 2"* ]]

    # Subject folders created at root; notes nested under them.
    FOLDER_CNT=$(sqlite3 "$DAAT_DB" "SELECT COUNT(*) FROM document WHERE parent_id IS NULL AND title IN ('daat', 'benchling') AND archived_at IS NULL;")
    [ "$FOLDER_CNT" -eq 2 ]

    SYNC_DOC_CONTENT=$(sqlite3 "$DAAT_DB" "SELECT content FROM document WHERE title='Sync Design' AND archived_at IS NULL;")
    [[ "$SYNC_DOC_CONTENT" == *"[[daat/Architecture]]"* ]]

    # Wikilink resolved in daat's document_link table!
    LINK_RESOLVED=$(sqlite3 "$DAAT_DB" "SELECT COUNT(*) FROM document_link WHERE to_document_id IS NOT NULL AND archived_at IS NULL;")
    [ "$LINK_RESOLVED" -eq 1 ]

    # Task due_to normalized to YYYY-MM-DD in daat.
    DAAT_TASK=$(sqlite3 "$DAAT_DB" "SELECT subject || '|' || due_to || '|' || importance || '|' || urgency || '|' || status FROM task WHERE archived_at IS NULL;")
    [ "$DAAT_TASK" = "benchling|2026-12-01|5.0|4.0|open" ]

    # Immediate second sync does 0 writes.
    run brex sync
    [ "$status" -eq 0 ]
    [[ "$output" == *"pushed 0 doc(s)/0 task(s), pulled 0 doc(s)/0 task(s)"* ]]
}

@test "daat UI/agent creations, Knowledge Pool distillations, and task edits pull back into brex while skipping chat_session transcripts" {
    brex task add -s daat -t "Wire curation loop" -c "Initial spec." -p 3 -u 3
    run brex sync
    [ "$status" -eq 0 ]

    DAAT_FOLDER_ID=$(sqlite3 "$DAAT_DB" "SELECT id FROM document WHERE parent_id IS NULL AND title='daat';")
    TASK_ID=$(sqlite3 "$DAAT_DB" "SELECT id FROM task WHERE archived_at IS NULL LIMIT 1;")

    # 1. Create a new note + task directly in daat under 'daat' folder.
    CREATE_DOC_OUT=$(daat_api_write POST "/api/v1/document" "{\"title\":\"UI Note\",\"content\":\"Connected to [[Wire curation loop]].\",\"parent_id\":$DAAT_FOLDER_ID}")
    NEW_DOC_ID=$(printf '%s' "$CREATE_DOC_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    daat_api_write POST "/api/v1/task" "{\"document\":$NEW_DOC_ID,\"subject\":\"daat\",\"due_to\":\"2026-12-15\",\"importance\":4,\"urgency\":5,\"status\":\"open\"}" >/dev/null

    # 2. Mark the existing task done in daat.
    daat_api_write PATCH "/api/v1/task/$TASK_ID" '{"status":"done"}' >/dev/null

    # 3. Simulate daat's AI agent writing a distilled note AND a chat_session transcript under "Knowledge Pool".
    KP_OUT=$(daat_api_write POST "/api/v1/document" '{"title":"Knowledge Pool"}')
    KP_ID=$(printf '%s' "$KP_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    DISTILL_OUT=$(daat_api_write POST "/api/v1/document" "{\"title\":\"Distilled Curation Principles\",\"content\":\"Key synthesis linking [[daat/UI Note]].\",\"parent_id\":$KP_ID}")
    DISTILL_ID=$(printf '%s' "$DISTILL_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    sqlite3 "$DAAT_DB" "UPDATE document SET source_type='distilled' WHERE id=$DISTILL_ID;"

    CHAT_OUT=$(daat_api_write POST "/api/v1/document" "{\"title\":\"Chat transcript 2026-11-19\",\"content\":\"User: hi\",\"parent_id\":$KP_ID}")
    CHAT_ID=$(printf '%s' "$CHAT_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    sqlite3 "$DAAT_DB" "UPDATE document SET source_type='chat_session' WHERE id=$CHAT_ID;"

    run brex sync
    [ "$status" -eq 0 ]

    # UI Note + its task pulled into brex.
    UI_TASK=$(sqlite3 "$BREX_DB" "SELECT n.subject || '|' || t.importance || '|' || t.urgency || '|' || substr(t.due_to,1,10) FROM notes n JOIN tasks t ON t.item_id=n.id WHERE n.title='UI Note';")
    [ "$UI_TASK" = "daat|4|5|2026-12-15" ]

    # Existing task marked done in brex.
    DONE_CNT=$(sqlite3 "$BREX_DB" "SELECT COUNT(*) FROM notes n JOIN tasks t ON t.item_id=n.id WHERE n.title='Wire curation loop' AND t.done IS NOT NULL;")
    [ "$DONE_CNT" -eq 1 ]

    # Distilled Knowledge Pool doc pulled under subject 'knowledge-pool' and searchable via brex knowledge search!
    KP_SUBJ=$(sqlite3 "$BREX_DB" "SELECT subject FROM notes WHERE title='Distilled Curation Principles';")
    [ "$KP_SUBJ" = "knowledge-pool" ]

    run brex knowledge search "Distilled Curation Principles"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Distilled Curation Principles"* ]]

    # Raw chat_session transcript NOT pulled into brex notes!
    CHAT_CNT=$(sqlite3 "$BREX_DB" "SELECT COUNT(*) FROM notes WHERE title='Chat transcript 2026-11-19';")
    [ "$CHAT_CNT" -eq 0 ]
}

@test "independent doc+task edits merge cleanly, concurrent body edits preserve both texts, and delete/archive sync both ways" {
    brex task add -s ops -t "Deploy notes VM" -c "Step 1." -p 4 -u 3
    brex note add -s ops -t "Obsolete Local" -c "Delete me locally."
    brex note add -s ops -t "Obsolete Remote" -c "Archive me in daat."
    run brex sync
    [ "$status" -eq 0 ]

    DEPLOY_DOC_ID=$(sqlite3 "$DAAT_DB" "SELECT id FROM document WHERE title='Deploy notes VM';")
    REMOTE_OBS_ID=$(sqlite3 "$DAAT_DB" "SELECT id FROM document WHERE title='Obsolete Remote';")

    # Independent edit: daat edits body of 'Deploy notes VM' while brex marks the task done!
    daat_api_write PATCH "/api/v1/document/$DEPLOY_DOC_ID" '{"content":"Step 1.\nDistilled details from daat agent."}' >/dev/null
    DEPLOY_BREX_ID=$(sqlite3 "$BREX_DB" "SELECT id FROM notes WHERE title='Deploy notes VM';")
    sqlite3 "$BREX_DB" "UPDATE tasks SET done='2026-11-19 20:00:00' WHERE item_id=$DEPLOY_BREX_ID;"

    # Delete 'Obsolete Local' in brex; archive 'Obsolete Remote' in daat.
    sqlite3 "$BREX_DB" "DELETE FROM notes WHERE title='Obsolete Local';"
    daat_api_write POST "/api/v1/document/$REMOTE_OBS_ID/archive" "" >/dev/null

    run brex sync
    [ "$status" -eq 0 ]
    [[ "$output" == *"merged 0"* ]]

    # Both independent edits landed cleanly!
    BREX_BODY=$(sqlite3 "$BREX_DB" "SELECT content FROM notes WHERE id=$DEPLOY_BREX_ID;")
    [[ "$BREX_BODY" == *"Distilled details from daat agent."* ]]
    DAAT_STATUS=$(sqlite3 "$DAAT_DB" "SELECT status FROM task WHERE document=$DEPLOY_DOC_ID;")
    [ "$DAAT_STATUS" = "done" ]

    # Archive/delete propagated in both directions.
    LOCAL_ARCHIVED=$(sqlite3 "$DAAT_DB" "SELECT COUNT(*) FROM document WHERE title='Obsolete Local' AND archived_at IS NOT NULL;")
    [ "$LOCAL_ARCHIVED" -eq 1 ]
    REMOTE_DELETED=$(sqlite3 "$BREX_DB" "SELECT COUNT(*) FROM notes WHERE title='Obsolete Remote';")
    [ "$REMOTE_DELETED" -eq 0 ]

    # Now trigger a true concurrent body edit on 'Deploy notes VM'.
    brex note add -s ops -t "Deploy notes VM" -c "CLI follow-up note." -u
    daat_api_write PATCH "/api/v1/document/$DEPLOY_DOC_ID" '{"content":"Concurrent daat agent rewrite."}' >/dev/null

    run brex sync
    [ "$status" -eq 0 ]
    [[ "$output" == *"merged 1"* ]]

    MERGED_BREX=$(sqlite3 "$BREX_DB" "SELECT content FROM notes WHERE id=$DEPLOY_BREX_ID;")
    MERGED_DAAT=$(sqlite3 "$DAAT_DB" "SELECT content FROM document WHERE id=$DEPLOY_DOC_ID;")
    [[ "$MERGED_BREX" == *"CLI follow-up note."* ]]
    [[ "$MERGED_BREX" == *"Concurrent daat agent rewrite."* ]]
    [ "$MERGED_BREX" = "$MERGED_DAAT" ]
}

@test "repo-synced folders and markdown docs are skipped on pull while sharing top-level repo folders and pulling Knowledge Pool connections" {
    brex note add -s daat -t "Personal daat note" -c "My design thoughts on curation."
    run brex sync
    [ "$status" -eq 0 ]

    DAAT_FOLDER_ID=$(sqlite3 "$DAAT_DB" "SELECT id FROM document WHERE parent_id IS NULL AND title='daat';")

    # Simulate repo_docs_sync adding daat/doc/architecture.md under existing top-level 'daat' folder,
    # plus a brand-new 'software' repo folder with AGENTS.md.
    DOC_SUB_OUT=$(daat_api_write POST "/api/v1/document" "{\"title\":\"doc\",\"parent_id\":$DAAT_FOLDER_ID,\"external_id\":\"repo-folder:daat/doc\"}")
    DOC_SUB_ID=$(printf '%s' "$DOC_SUB_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    daat_api_write POST "/api/v1/document" "{\"title\":\"architecture\",\"content\":\"> **Repo:** \`daat/doc/architecture.md\`\n\nSystem architecture.\",\"parent_id\":$DOC_SUB_ID,\"external_id\":\"repo:daat:doc/architecture.md\"}" >/dev/null

    SW_ROOT_OUT=$(daat_api_write POST "/api/v1/document" '{"title":"software","external_id":"repo-folder:software"}')
    SW_ROOT_ID=$(printf '%s' "$SW_ROOT_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    daat_api_write POST "/api/v1/document" "{\"title\":\"AGENTS\",\"content\":\"> **Repo:** \`software/AGENTS.md\`\n\nAgent rules.\",\"parent_id\":$SW_ROOT_ID,\"external_id\":\"repo:software:AGENTS.md\"}" >/dev/null

    # Simulate a Knowledge Pool connection document linking the repo doc and personal note.
    KP_OUT=$(daat_api_write POST "/api/v1/document" '{"title":"Knowledge Pool"}')
    KP_ID=$(printf '%s' "$KP_OUT" | sed -n 's/.*"created_id":\([0-9]*\).*/\1/p')
    daat_api_write POST "/api/v1/document" "{\"title\":\"architecture ↔ Personal daat note\",\"content\":\"Connects [[architecture]] with [[Personal daat note]].\",\"parent_id\":$KP_ID}" >/dev/null

    # Also add a personal note in brex with subject 'software' to verify it shares the top-level 'software' folder.
    brex note add -s software -t "Personal software note" -c "Follow up on pipeline."

    run brex sync
    [ "$status" -eq 0 ]
    [[ "$output" == *"pushed 1 doc(s)/0 task(s), pulled 1 doc(s)/0 task(s)"* ]]

    # Raw repo docs and repo subfolders were NOT pulled into brex!
    REPO_PULLED=$(sqlite3 "$BREX_DB" "SELECT COUNT(*) FROM notes WHERE title IN ('doc', 'architecture', 'AGENTS', 'software');")
    [ "$REPO_PULLED" -eq 0 ]

    # Knowledge Pool connection doc WAS pulled into brex!
    KP_PULLED=$(sqlite3 "$BREX_DB" "SELECT COUNT(*) FROM notes WHERE subject='knowledge-pool' AND title='architecture ↔ Personal daat note';")
    [ "$KP_PULLED" -eq 1 ]

    # Personal software note reused the existing top-level 'software' folder in daat!
    SW_NOTE_PARENT=$(sqlite3 "$DAAT_DB" "SELECT parent_id FROM document WHERE title='Personal software note' AND archived_at IS NULL;")
    [ "$SW_NOTE_PARENT" = "$SW_ROOT_ID" ]

    # Repo docs remain active and untouched in daat.
    ACTIVE_REPO_DOCS=$(sqlite3 "$DAAT_DB" "SELECT COUNT(*) FROM document WHERE external_id LIKE 'repo:%' AND archived_at IS NULL;")
    [ "$ACTIVE_REPO_DOCS" -eq 2 ]
}
