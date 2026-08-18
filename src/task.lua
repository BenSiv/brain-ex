-- Define a module table
task = {}

os = require("os")
utils = require("utils")
argparse = require("argparse")
database = require("database")
local_update = database.sqlite_update
local_query = database.sqlite_query
config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
help = require("help")
dates = require("dates")
bx_utils = require("bx_utils")
generate_id = bx_utils.generate_id
dataframes = require("dataframes")
view = dataframes.view
sync = require("sync")
paths = require("paths")
sql_schema = require("sql_schema")
note = require("note")
prettyprint = require("prettyprint")

-- A task is a `notes` row with a matching `tasks` row attached by
-- id -- there is no separate content/subject/title on tasks at
-- all. See doc/unified-items-design.md.

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function column_exists(brain_file, table_name, column_name)
    query = "PRAGMA table_info(" .. table_name .. ");"
    columns = database.sqlite_query(brain_file, query)
    if columns == nil then
        return false
    end
    for _, col in pairs(columns) do
        if col["name"] == column_name or col[2] == column_name then
            return true
        end
    end
    return false
end

function table_exists(brain_file, table_name)
    query = string.format("SELECT name FROM sqlite_master WHERE type='table' AND name='%s';", table_name)
    result = database.sqlite_query(brain_file, query)
    return result != nil and #result > 0
end

function ensure_notes_id_column(brain_file)
    if not table_exists(brain_file, "notes") then
        return
    end
    if not column_exists(brain_file, "notes", "id") then
        database.sqlite_update(brain_file, "ALTER TABLE notes ADD COLUMN id INTEGER;")
    end
end

-- One-time, idempotent: folds a pre-unification `tasks` table into
-- notes + tasks, preserving each task's original id (content
-- elsewhere already refers to tasks by that number). Never DROPs the
-- old table -- renames it to tasks_legacy_backup as a safety net.
--
-- The tracking-only replacement table is deliberately also named
-- `tasks` (no separate `tasks`/`task_meta` split -- it's the only
-- "tasks" table there is now), which means an old-shape `tasks` table
-- (with its own content/subject/comment columns) occupies the exact
-- name the new one needs. `CREATE TABLE IF NOT EXISTS` in ensure_schema
-- silently no-ops against that old table rather than creating the new
-- shape, so this function has to detect that case (a `content` column
-- is the tell -- the new shape never has one) and rename the old table
-- out of the way *before* the new `tasks` table can be created at all.
function migrate_legacy_tasks(brain_file)
    if not table_exists(brain_file, "tasks") then
        return true
    end
    if not column_exists(brain_file, "tasks", "content") then
        return true -- already the new, tracking-only shape
    end

    database.sqlite_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    flag = database.sqlite_query(brain_file, "SELECT value FROM sync_meta WHERE key='tasks_migrated_to_notes';")
    if flag != nil and #flag > 0 then
        return true
    end

    database.sqlite_update(brain_file, "ALTER TABLE tasks RENAME TO tasks_legacy_backup;")
    database.sqlite_update(brain_file, sql_schema.sql_init)
    ensure_notes_id_column(brain_file)

    rows = database.sqlite_query(brain_file, "SELECT id, time, content, subject, due_to, overdue, done, comment, owner, importance, urgency FROM tasks_legacy_backup;")
    if rows == nil then
        rows = {}
    end

    for _, row in ipairs(rows) do
        id = row.id
        if id == nil then id = row[1] end
        content_val = row.content
        if content_val == nil then content_val = row[3] end
        subject_val = row.subject
        if subject_val == nil then subject_val = row[4] end
        due_to_val = row.due_to
        if due_to_val == nil then due_to_val = row[5] end
        overdue_val = row.overdue
        if overdue_val == nil then overdue_val = row[6] end
        done_val = row.done
        if done_val == nil then done_val = row[7] end
        comment_val = row.comment
        if comment_val == nil then comment_val = row[8] end
        owner_val = row.owner
        if owner_val == nil then owner_val = row[9] end
        importance_val = row.importance
        if importance_val == nil then importance_val = row[10] end
        urgency_val = row.urgency
        if urgency_val == nil then urgency_val = row[11] end

        if content_val == nil or content_val == "" or content_val == "NULL" then
            content_val = "(untitled task " .. tostring(id) .. ")"
        end
        title_val = content_val

        subject_for_note = subject_val
        if subject_for_note == nil or subject_for_note == "" or subject_for_note == "NULL" then
            subject_for_note = "tasks"
        end

        -- notes has a real UNIQUE(title, subject) that a bare numeric
        -- task id was never subject to before -- disambiguate a
        -- collision deterministically instead of letting the insert fail.
        esc_subject = escape_sql(subject_for_note)
        esc_title = escape_sql(title_val)
        collision = database.sqlite_query(brain_file, string.format(
            "SELECT COUNT(*) AS cnt FROM notes WHERE subject='%s' AND title='%s';", esc_subject, esc_title))
        has_collision = false
        if collision != nil and collision[1] != nil then
            cnt = collision[1].cnt
            if cnt == nil then cnt = collision[1][1] end
            if tonumber(cnt) != nil and tonumber(cnt) > 0 then
                has_collision = true
            end
        end
        if has_collision then
            title_val = title_val .. " [" .. tostring(id) .. "]"
        end

        body = ""
        if comment_val != nil and comment_val != "" and comment_val != "NULL" then
            body = "DONE: " .. comment_val
        end

        database.sqlite_update(brain_file, string.format(
            "INSERT INTO notes (id, subject, title, content) VALUES ('%s', '%s', '%s', '%s');",
            escape_sql(tostring(id)), escape_sql(subject_for_note), escape_sql(title_val), escape_sql(body)
        ))

        esc_due_to = "NULL"
        if due_to_val != nil and due_to_val != "" and due_to_val != "NULL" then
            esc_due_to = "'" .. escape_sql(due_to_val) .. "'"
        end
        overdue_num = tonumber(overdue_val)
        if overdue_num == nil then overdue_num = 0 end
        esc_done = "NULL"
        if done_val != nil and done_val != "" and done_val != "NULL" then
            esc_done = "'" .. escape_sql(done_val) .. "'"
        end
        esc_owner = "NULL"
        if owner_val != nil and owner_val != "" and owner_val != "NULL" then
            esc_owner = "'" .. escape_sql(owner_val) .. "'"
        end
        importance_num = tonumber(importance_val)
        if importance_num == nil then importance_num = 1 end
        urgency_num = tonumber(urgency_val)
        if urgency_num == nil then urgency_num = 1 end

        database.sqlite_update(brain_file, string.format(
            "INSERT INTO tasks (item_id, due_to, overdue, done, owner, importance, urgency) VALUES ('%s', %s, %d, %s, %s, %d, %d);",
            escape_sql(tostring(id)), esc_due_to, overdue_num, esc_done, esc_owner, importance_num, urgency_num
        ))
    end

    database.sqlite_update(brain_file, "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('tasks_migrated_to_notes', '1');")
    return true
end

function ensure_schema(brain_file)
    database.sqlite_update(brain_file, sql_schema.sql_init)
    ensure_notes_id_column(brain_file)
    migrate_legacy_tasks(brain_file)
end

-- notes.subject is always a real string, never SQL NULL -- note.lua's
-- own insert paths and vault_to_sql.lua both default a missing
-- subject to "" (never nil), so matching that convention here (rather
-- than IS NULL) is what actually finds/collides with existing rows.
function normalize_subject(subject)
    if subject == nil then
        return ""
    end
    return subject
end

function subject_predicate(subject)
    return "subject = '" .. escape_sql(normalize_subject(subject)) .. "'"
end

-- Returns the id for an existing (subject, title) note, backfilling
-- one on demand if the row predates the id column. nil if no such note.
function get_note_id(brain_file, subject, title)
    query = string.format("SELECT id FROM notes WHERE title='%s' AND %s;", escape_sql(title), subject_predicate(subject))
    result = database.sqlite_query(brain_file, query)
    if result == nil or #result == 0 then
        return nil
    end
    row = result[1]
    item_id = row.id
    if item_id == nil then item_id = row[1] end
    if item_id == nil or item_id == "" then
        item_id = bx_utils.generate_id("notes", nil, nil, brain_file)
        database.sqlite_update(brain_file, string.format(
            "UPDATE notes SET id='%s' WHERE title='%s' AND %s;", item_id, escape_sql(title), subject_predicate(subject)))
    end
    return tostring(item_id)
end

function get_item_subject_title(brain_file, item_id)
    result = database.sqlite_query(brain_file, string.format("SELECT subject, title FROM notes WHERE id='%s';", escape_sql(tostring(item_id))))
    if result == nil or #result == 0 then
        return nil, nil
    end
    row = result[1]
    subj = row.subject
    if subj == nil then subj = row[1] end
    ttl = row.title
    if ttl == nil then ttl = row[2] end
    return subj, ttl
end

-- Find-or-create on notes: "start tracking this note as a task" and
-- "create a new task" are the same operation, since a task is nothing
-- but a note with tasks attached. Never overwrites an existing
-- note's content -- an initial_content on an already-existing note is
-- appended as a comment instead.
function find_or_create_note(brain_file, subject, title, initial_content)
    subject = normalize_subject(subject)
    existing_id = get_note_id(brain_file, subject, title)
    if existing_id != nil then
        if initial_content != nil and initial_content != "" then
            note.append_content(brain_file, subject, title, initial_content)
        end
        return existing_id
    end

    new_id = bx_utils.generate_id("notes", nil, nil, brain_file)
    esc_subject = "'" .. escape_sql(subject) .. "'"
    body = initial_content
    if body == nil then
        body = ""
    end
    insert_stmt = string.format(
        "INSERT INTO notes (id, subject, title, content) VALUES ('%s', %s, '%s', '%s');",
        new_id, esc_subject, escape_sql(title), escape_sql(body)
    )
    status = database.sqlite_update(brain_file, insert_stmt)
    if status == nil then
        return nil
    end
    return new_id
end

function check_overdue(due_to)
    if due_to == nil or due_to == "" then
        return false
    end
    normalized = dates.normalize_datetime(due_to)
    if normalized == nil then
        return false
    end
    if dates.is_valid_timestamp  !=  nil and dates.is_valid_timestamp(normalized) == false then
        return false
    end
    -- Use fixed-width slicing to avoid pattern issues in luam
    year = tonumber(string.sub(normalized, 1, 4))
    month = tonumber(string.sub(normalized, 6, 7))
    day = tonumber(string.sub(normalized, 9, 10))
    hour = tonumber(string.sub(normalized, 12, 13))
    min = tonumber(string.sub(normalized, 15, 16))
    sec = tonumber(string.sub(normalized, 18, 19))
    if year == nil or month == nil or day == nil or hour == nil or min == nil or sec == nil then
        return false
    end
    current_time = os.time()
    task_time = os.time({year=year, month=month, day=day, hour=hour, min=min, sec=sec})
    if task_time == nil then
        return false
    end
    return current_time > task_time
end

function update_overdue(brain_file)
    query = "SELECT item_id, due_to FROM tasks WHERE done IS NULL AND due_to IS NOT NULL;"
    unfinished = database.sqlite_query(brain_file, query)

    overdue = false
    update_statement = ""
    if unfinished  !=  nil then
        for _, task_row in pairs(unfinished) do
            task_id = task_row[1]
            if task_row.item_id != nil then
                task_id = task_row.item_id
            end
            task_due = task_row[2]
            if task_row.due_to != nil then
                task_due = task_row.due_to
            end
            overdue = check_overdue(task_due)
            if overdue then
                if task_id  !=  nil then
                    update_statement = "UPDATE tasks SET overdue = 1 WHERE item_id = " .. task_id .. ";"
                else
                    update_statement = nil
                end
                success = database.sqlite_update(brain_file, update_statement)
                if success == nil then
                    return nil, "Failed to update overdue status for task ID: " .. tostring(task_id)
                end
            end
        end
    end
    return true
end

-- DB -> vault direction, for one item (or all task-tracked items when
-- task_id is nil, e.g. `task delay --id "*"`). Tasks live at the same
-- <subject>/<title>.md file a plain note would, since promoting an
-- existing note into a task must never create a second, divergent
-- copy of the same content -- so this writes via note.lua's own path
-- helper, not a separate directory tree.
function backup_task(brain_file, task_id)
    vault_path = get_vault_path()
    if vault_path == nil then
        return true
    end

    row_result = database.sqlite_query(brain_file, string.format("""
        SELECT notes.id, notes.time, notes.subject, notes.title, notes.content,
               tasks.due_to, tasks.overdue, tasks.done,
               tasks.owner, tasks.importance, tasks.urgency
        FROM notes JOIN tasks ON tasks.item_id = notes.id
        WHERE notes.id = '%s';
    """, escape_sql(tostring(task_id))))
    if row_result == nil or #row_result == 0 then
        return true
    end
    row = row_result[1]

    content = row.content
    if content == nil then
        content = ""
    end
    subject = row.subject
    if subject != nil and subject == "" then
        subject = nil
    end

    metadata = {
        id = row.id,
        title = row.title,
        time = row.time,
        is_task = "true",
        due_to = row.due_to,
        overdue = row.overdue,
        done = row.done,
        owner = row.owner,
        importance = row.importance,
        urgency = row.urgency
    }
    frontmatter = bx_utils.serialize_frontmatter(metadata)

    note_dir, note_path = note.get_note_paths(vault_path, subject, row.title)
    bx_utils.ensure_dir(note_dir)

    f = io.open(note_path, "w")
    if f == nil then
        return nil, "Could not write task file: " .. note_path
    end
    io.write(f, frontmatter)
    io.write(f, content)
    io.close(f)
    return true
end

function persist_task(brain_file, task_id)
    if task_id == nil or task_id == "*" then
        -- Wildcard/unscoped write touched an unknown set of tasks --
        -- fall back to a full vault resync.
        return sync.refresh(brain_file)
    end

    status, err = backup_task(brain_file, task_id)
    if status == nil then
        return nil, err
    end
    return true
end

function add_task(brain_file, args)
    ensure_schema(brain_file)

    subject = args["subject"]
    title = args["title"]
    content = args["content"]
    owner = args["owner"]

    if title == nil or title == "" then
        return nil, "Must provide task title"
    end

    importance = 1
    if tonumber(args["importance"]) != nil then
        importance = tonumber(args["importance"])
    end
    urgency = 1
    if tonumber(args["urgency"]) != nil then
        urgency = tonumber(args["urgency"])
    end

    if importance < 1 or importance > 5 then
        return nil, "Importance must be an integer between 1 and 5"
    end
    if urgency < 1 or urgency > 5 then
        return nil, "Urgency must be an integer between 1 and 5"
    end

    due_to = nil
    if args["due_to"] != nil then
        due_to = dates.normalize_datetime(args["due_to"])
        if due_to == nil then
            return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
        end
    end

    item_id = find_or_create_note(brain_file, subject, title, content)
    if item_id == nil then
        return nil, "Failed to add task"
    end

    overdue_bool = false
    if due_to != nil then
        overdue_bool = check_overdue(due_to)
    end
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end

    esc_owner = "NULL"
    if owner  !=  nil and owner  !=  "" then
        esc_owner = "'" .. escape_sql(owner) .. "'"
    end
    esc_due_to = "NULL"
    if due_to != nil then
        esc_due_to = "'" .. escape_sql(due_to) .. "'"
    end

    -- Upsert, not a plain insert: re-running `task add` against an
    -- already-tracked task just updates its tracking fields, same
    -- effect prioritize/delay have individually.
    upsert_statement = string.format("""
    INSERT INTO tasks (item_id, due_to, overdue, done, owner, importance, urgency)
    VALUES ('%s', %s, '%s', NULL, %s, %d, %d)
    ON CONFLICT(item_id) DO UPDATE SET
        due_to = excluded.due_to,
        overdue = excluded.overdue,
        owner = excluded.owner,
        importance = excluded.importance,
        urgency = excluded.urgency;
    """, item_id, esc_due_to, esc_overdue, esc_owner, importance, urgency)

    success = database.sqlite_update(brain_file, upsert_statement)
    if success == nil then
        return nil, "Failed to add task"
    end

    return persist_task(brain_file, item_id)
end

function list_tasks(brain_file, args)
    ensure_schema(brain_file)
    tasks_empty = bx_utils.is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("No pending tasks")
        return true
    end

    update_overdue(brain_file)

    subject = ""
    if args["subject"] != nil then
        subject = args["subject"]
    end
    owner = ""
    if args["owner"] != nil then
        owner = args["owner"]
    end

    due_to = nil
    if args["due_to"] != nil then
        due_to = dates.normalize_datetime(args["due_to"])
        if due_to == nil then
            return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
        end
    end

    query = """
    SELECT
        notes.id AS id,
        notes.title AS title,
        notes.subject AS subject,
        tasks.due_to AS due_to,
        tasks.overdue AS overdue,
        tasks.done AS done,
        tasks.owner AS owner,
        COALESCE(tasks.importance, 1) AS importance,
        COALESCE(tasks.urgency, 1) AS manual_urgency,
        COALESCE(
            CASE
                WHEN tasks.due_to IS NULL THEN COALESCE(tasks.urgency, 1)
                ELSE CASE
                    WHEN (julianday(tasks.due_to) - julianday('now', 'localtime')) <= 1 THEN 5
                    WHEN (julianday(tasks.due_to) - julianday('now', 'localtime')) <= 2 THEN
                        CASE WHEN COALESCE(tasks.urgency, 1) > 4 THEN COALESCE(tasks.urgency, 1) ELSE 4 END
                    WHEN (julianday(tasks.due_to) - julianday('now', 'localtime')) <= 4 THEN
                        CASE WHEN COALESCE(tasks.urgency, 1) > 3 THEN COALESCE(tasks.urgency, 1) ELSE 3 END
                    WHEN (julianday(tasks.due_to) - julianday('now', 'localtime')) <= 7 THEN
                        CASE WHEN COALESCE(tasks.urgency, 1) > 2 THEN COALESCE(tasks.urgency, 1) ELSE 2 END
                    ELSE COALESCE(tasks.urgency, 1)
                END
            END,
            1
        ) AS active_urgency
    FROM notes
    JOIN tasks ON tasks.item_id = notes.id
    WHERE tasks.done IS NULL
    """

    if subject  !=  "" then
        query = query .. string.format(" AND notes.subject = '%s'", escape_sql(subject))
    end

    if owner  !=  "" then
        query = query .. string.format(" AND tasks.owner = '%s' ", escape_sql(owner))
    end

    if due_to  !=  nil then
        query = query .. string.format(" AND tasks.due_to > '%s'", due_to)
    end

    query = query .. """
     ORDER BY
        CASE
            WHEN COALESCE(importance, 1) >= 4 AND active_urgency >= 4 THEN 1
            WHEN COALESCE(importance, 1) >= 4 AND active_urgency < 4 THEN 2
            WHEN COALESCE(importance, 1) < 4 AND active_urgency >= 4 THEN 3
            ELSE 4
        END ASC,
        (active_urgency * COALESCE(importance, 1)) DESC,
        COALESCE(importance, 1) DESC,
        active_urgency DESC,
        CASE WHEN due_to IS NULL THEN 1 ELSE 0 END ASC,
        due_to ASC,
        subject ASC;
    """

    result = database.sqlite_query(brain_file, query)
    if result  !=  nil and utils.length(result) > 0 then
        -- Load settings
        settings = config.load_settings()

        -- Resolve colors
        defaults = {
            Q1 = "\027[31m",
            Q2 = "\027[38;5;208m",
            Q3 = "\027[33m",
            Q4 = "\027[90m",
            reset = "\027[0m"
        }
        user_colors = settings.colors
        if user_colors == nil then
            user_colors = {}
        end
        Q1_color = defaults.Q1
        if user_colors.Q1 != nil then
            Q1_color = user_colors.Q1
        end
        Q2_color = defaults.Q2
        if user_colors.Q2 != nil then
            Q2_color = user_colors.Q2
        end
        Q3_color = defaults.Q3
        if user_colors.Q3 != nil then
            Q3_color = user_colors.Q3
        end
        Q4_color = defaults.Q4
        if user_colors.Q4 != nil then
            Q4_color = user_colors.Q4
        end
        reset_color = defaults.reset
        if user_colors.reset != nil then
            reset_color = user_colors.reset
        end
        colors = {
            Q1 = Q1_color,
            Q2 = Q2_color,
            Q3 = Q3_color,
            Q4 = Q4_color,
            reset = reset_color
        }

        -- Resolve columns
        cols = settings.task_columns

        -- Handle hide_due_to deprecation warning
        hide_due = false
        cfg = config.get_config()
        if cfg  !=  nil and cfg["hide_due_to"]  !=  nil then
            io.write(io.stderr, "Warning: 'hide_due_to' configuration in config.yaml is deprecated. Please define 'task_columns' in settings.json instead.\n")
            if cfg["hide_due_to"] == true or cfg["hide_due_to"] == "true" then
                hide_due = true
            end
        end

        if cols == nil then
            -- Fallback to default logic
            cols = {"id", "priority", "subject", "title"}
            if not hide_due then
                table.insert(cols, "due_to")
            end
        else
            if hide_due then
                filtered_cols = {}
                for _, col in ipairs(cols) do
                    if col != "due_to" then
                        table.insert(filtered_cols, col)
                    end
                end
                cols = filtered_cols
            end
        end

        for _, task_row in ipairs(result) do
            imp = 1
            if tonumber(task_row.importance) != nil then
                imp = tonumber(task_row.importance)
            end
            urg = 1
            if tonumber(task_row.active_urgency) != nil then
                urg = tonumber(task_row.active_urgency)
            end

            quadrant = 4
            color_code = colors.Q4
            if imp >= 4 and urg >= 4 then
                quadrant = 1
                color_code = colors.Q1
            elseif imp >= 4 and urg < 4 then
                quadrant = 2
                color_code = colors.Q2
            elseif imp < 4 and urg >= 4 then
                quadrant = 3
                color_code = colors.Q3
            end

            reset_code = colors.reset
            priority_text = "Q" .. quadrant .. " (I:" .. imp .. " U:" .. urg .. ")"

            -- If due_to is overdue, we want to show [OVERDUE] tag
            due_to_val = task_row.due_to
            if due_to_val == nil then
                due_to_val = ""
            end
            due_str = tostring(due_to_val)
            if task_row.overdue == 1 or task_row.overdue == "1" then
                due_str = due_str .. " [OVERDUE]"
            end

            -- Apply the priority color to the selected columns for this row
            for _, col in ipairs(cols) do
                val = task_row[col]
                if col == "priority" then
                    val = priority_text
                elseif col == "due_to" then
                    val = due_str
                else
                    if val == nil then
                        val = ""
                    end
                    val = tostring(val)
                end
                task_row[col] = color_code .. val .. reset_code
            end
        end
        view(result, {columns=cols, line_length=999})
    else
        print("No pending tasks")
    end
    return true
end

function mark_done(brain_file, args)
    task_id = ""
    if args["id"] != nil then
        task_id = args["id"]
    end
    comment = ""
    if args["comment"] != nil then
        comment = args["comment"]
    end

    if task_id == "" then
        return nil, "Must provide task id"
    end

    update_statement = "UPDATE tasks SET done = CURRENT_TIMESTAMP WHERE item_id = " .. task_id .. ";"
    status = database.sqlite_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to mark task as done"
    end

    if comment != "" then
        subject, title = get_item_subject_title(brain_file, task_id)
        if title != nil then
            note.append_content(brain_file, subject, title, os.date("%Y-%m-%d %H:%M:%S") .. "\nDONE: " .. comment)
        end
    end

    return persist_task(brain_file, task_id)
end

-- Ongoing comments, not just at done time: appends a timestamped
-- entry to the task's own note content -- the same append_content
-- notes have always had, just newly reachable for tasks too.
function comment_task(brain_file, args)
    task_id = ""
    if args["id"] != nil then
        task_id = args["id"]
    end
    comment = ""
    if args["comment"] != nil then
        comment = args["comment"]
    end

    if task_id == "" then
        return nil, "Must provide task id"
    end
    if comment == "" then
        return nil, "Must provide comment content"
    end

    subject, title = get_item_subject_title(brain_file, task_id)
    if title == nil then
        return nil, "No such task: " .. tostring(task_id)
    end

    status, err = note.append_content(brain_file, subject, title, os.date("%Y-%m-%d %H:%M:%S") .. "\n" .. comment)
    if status == nil then
        return nil, err
    end

    return persist_task(brain_file, task_id)
end

-- The "drill into one task" view -- since `list` now only shows
-- titles, this is where the full comment log lives.
function show_task(brain_file, args)
    task_id = ""
    if args["id"] != nil then
        task_id = args["id"]
    end
    if task_id == "" then
        return nil, "Must provide task id"
    end

    query = string.format("""
        SELECT notes.id AS id, notes.title AS title, notes.subject AS subject, notes.content AS content,
               tasks.due_to AS due_to, tasks.overdue AS overdue, tasks.done AS done,
               tasks.owner AS owner, tasks.importance AS importance, tasks.urgency AS urgency
        FROM notes JOIN tasks ON tasks.item_id = notes.id
        WHERE notes.id = '%s';
    """, escape_sql(tostring(task_id)))
    result = database.sqlite_query(brain_file, query)
    if result == nil or #result == 0 then
        print("No such task: " .. tostring(task_id))
        return true
    end
    row = result[1]

    prettyprint.bold(tostring(row.title))
    subject_display = row.subject
    if subject_display == nil or subject_display == "" then
        subject_display = "(none)"
    end
    print("id: " .. tostring(row.id) .. "  subject: " .. tostring(subject_display))
    print("importance: " .. tostring(row.importance) .. "  urgency: " .. tostring(row.urgency))
    due_display = row.due_to
    if due_display == nil or due_display == "" then
        due_display = "(none)"
    end
    print("due_to: " .. tostring(due_display) .. "  overdue: " .. tostring(row.overdue))
    done_display = row.done
    if done_display == nil or done_display == "" then
        done_display = "(not done)"
    end
    print("done: " .. tostring(done_display))
    print("")
    content_display = row.content
    if content_display == nil or content_display == "" then
        content_display = "(no comments yet)"
    end
    print(content_display)
    return true
end

function delay_due(brain_file, args)
    task_id = ""
    if args["id"] != nil then
        task_id = args["id"]
    end

    due_to = nil
    is_indefinite = false
    if args["due_to"] != nil then
        if args["due_to"] == "indefinitely" then
            is_indefinite = true
        else
            due_to = dates.normalize_datetime(args["due_to"])
            if due_to == nil then
                return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it, or 'indefinitely'"
            end
        end
    else
        current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    end

    overdue_bool = false
    if is_indefinite == false then
        overdue_bool = check_overdue(due_to)
    end
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end
    update_statement = nil
    if task_id == "*" then
        if is_indefinite then
            update_statement = "UPDATE tasks SET due_to=NULL, overdue=0 WHERE done IS NULL;"
        else
            update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, esc_overdue)
        end
    else
        if is_indefinite then
            update_statement = string.format("UPDATE tasks SET due_to=NULL, overdue=0 WHERE item_id='%s';", task_id)
        else
            update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE item_id='%s';", due_to, esc_overdue, task_id)
        end
    end
    status = database.sqlite_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to delay task due date"
    end
    return persist_task(brain_file, task_id)
end

function update_priority(brain_file, args)
    task_id = ""
    if args["id"] != nil then
        task_id = args["id"]
    end
    importance_str = args["importance"]
    urgency_str = args["urgency"]

    if task_id == "" then
        return nil, "Must provide task id"
    end

    if importance_str == nil and urgency_str == nil then
        return nil, "Must provide importance or urgency to update"
    end

    updates = {}
    if importance_str != nil then
        importance = tonumber(importance_str)
        if importance == nil or importance < 1 or importance > 5 then
            return nil, "Importance must be an integer between 1 and 5"
        end
        table.insert(updates, "importance=" .. importance)
    end

    if urgency_str != nil then
        urgency = tonumber(urgency_str)
        if urgency == nil or urgency < 1 or urgency > 5 then
            return nil, "Urgency must be an integer between 1 and 5"
        end
        table.insert(updates, "urgency=" .. urgency)
    end

    update_statement = nil
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET %s WHERE done IS NULL;", table.concat(updates, ", "))
    else
        update_statement = string.format("UPDATE tasks SET %s WHERE item_id='%s';", table.concat(updates, ", "), task_id)
    end

    status = database.sqlite_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to update task priority"
    end
    return persist_task(brain_file, task_id)
end

function last_done(brain_file, args)
    subject = ""
    if args["subject"] != nil then
        subject = args["subject"]
    end
    num = 5
    if args["number"] != nil then
        num = args["number"]
    end

    query = "SELECT notes.title AS title, notes.subject AS subject, notes.content AS content FROM notes JOIN tasks ON tasks.item_id = notes.id WHERE tasks.done IS NOT NULL "
    if subject  !=  "" then
        query = query .. string.format("AND notes.subject='%s'", escape_sql(subject))
    end

    query = query .. " ORDER BY tasks.done DESC "

    if num  !=  "" then
        query = query .. string.format("LIMIT %s", num)
    end

    result = database.sqlite_query(brain_file, query)
    if utils.length(result) > 0 then
        view(result, {columns={"subject", "title", "content"}})
    else
        print("No tasks to view")
    end
    return true
end

function do_task(brain_file, cmd_args)
    is_help = false
    for _, a in ipairs(cmd_args) do
        if a == "--help" or a == "-h" then
            is_help = true
            break
        end
    end

    if not is_help then
        ensure_schema(brain_file)
    end

    subcommand = cmd_args[1]
    if subcommand  !=  nil and string.sub(subcommand, 1, 1)  !=  "-" then
        valid_subs = {
            ["add"] = true,
            ["list"] = true,
            ["done"] = true,
            ["delay"] = true,
            ["prioritize"] = true,
            ["rank"] = true,
            ["comment"] = true,
            ["show"] = true,
            ["last"] = true
        }
        if valid_subs[subcommand] == nil then
            print("Unknown subcommand: " .. subcommand)
            print("Available subcommands: add, list, done, delay, prioritize, comment, show, last")
            return "success"
        end
        table.insert(cmd_args, 1, "-d")
    else
        subcommand = nil
    end

    arg_string = """
        -d --do arg string false
        -s --subject arg string false
        -t --title arg string false
        -e --due_to arg string false
        -i --id arg string false
        -m --comment arg string false
        -c --content arg string false
        -n --number arg number false
        -o --owner arg string false
        -p --importance arg string false
        -u --urgency arg string false
    """

    help_string = help.get_help_string(arg[0])
    expected_args = argparse.def_args(arg_string)
    args = argparse.parse_args(cmd_args, expected_args, help_string)
    if args == nil then
        return "success"
    end

    status, err = nil, nil
    if args  !=  nil then
        if args["do"] == "add" then
            status, err = add_task(brain_file, args)
        elseif args["do"] == "list" then
            status, err = list_tasks(brain_file, args)
        elseif args["do"] == "done" then
            status, err = mark_done(brain_file, args)
        elseif args["do"] == "delay" then
            status, err = delay_due(brain_file, args)
        elseif args["do"] == "prioritize" or args["do"] == "rank" then
            status, err = update_priority(brain_file, args)
        elseif args["do"] == "comment" then
            status, err = comment_task(brain_file, args)
        elseif args["do"] == "show" then
            status, err = show_task(brain_file, args)
        elseif args["do"] == "last" then
            status, err = last_done(brain_file, args)
        elseif args["do"] == nil then
            status, err = add_task(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, list, done, delay, prioritize, comment, show, last")
            return "success" -- Help printed
        end
    end
    if status  !=  true then
        if err != nil then
            print(err)
        else
            print("Task command failed")
        end
        return "error"
    end
    return "success"
end

task.do_task = do_task
task.add_task = add_task
task.list_tasks = list_tasks
task.mark_done = mark_done
task.comment_task = comment_task
task.show_task = show_task
task.delay_due = delay_due
task.update_priority = update_priority
task.last_done = last_done
task.backup_task = backup_task
task.persist_task = persist_task
task.ensure_schema = ensure_schema

if string.match(arg[0], "task.lua$")  !=  nil then
    do_task(get_brain_path(), arg)
else
    -- Export the module
    return task
end
