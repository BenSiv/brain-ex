-- update brain file from obsidian vault
vault_update = {}

utils = require("utils")
joinpath = require("paths").joinpath
database = require("database")
lfs = require("lfs")
bx_utils = require("bx_utils")

-- parse links string like "daily/note1,backend/note2,note3"
function parse_links_str(links_str)
    if links_str == nil or links_str == "" then
        return {}
    end

    links = {}
    for raw_link in string.gmatch(links_str, "[^,]+") do
        raw_link = strip(raw_link)  -- remove extra spaces
        parts = split(raw_link, "/")
        if #parts == 2 then
            table.insert(links, {subject=parts[1], title=parts[2]})
        else
            table.insert(links, {subject="", title=parts[1]})
        end
    end
    return links
end

function get_file_attributes(file_path)
    attr = lfs.attributes(file_path)
    if attr != nil then
        return os.date("%Y-%m-%d %H:%M:%S", attr.modification), attr.size
    else
        error("Could not get attributes for file: " .. file_path)
    end
end

function filter_markdown_files(dir_content)
    markdown_files = {}

    for _, entry in pairs(dir_content) do
        if string.match(entry, "%.md$") != nil then
            table.insert(markdown_files, entry)
        end
    end

    return markdown_files
end

function filter_directories(dir_path, dir_content)
    directories = {}

    for _, entry in pairs(dir_content) do
        entry_path = dir_path .. '/' .. entry
        attr = lfs.attributes(entry_path)
        if attr != nil and attr.mode == "directory" then
            if string.sub(entry, 1, 1) != '.' then
                table.insert(directories, entry)
            end
        end
    end

    return directories
end

function get_vault_files(vault_path)
    vault_content = {}
    dir_content = utils.readdir(vault_path)
    vault_content["root"] = filter_markdown_files(dir_content)
    vault_subjects = filter_directories(vault_path, dir_content)
    for _, subject in pairs(vault_subjects) do
        dir_content = utils.readdir(vault_path .. "/" .. subject)
        vault_content[subject] = filter_markdown_files(dir_content)
    end
    return vault_content
end

function read_note(vault_path, note)
    if note == nil then
        return nil
    end
    note_path = joinpath(vault_path, note)
    note_name = string.gsub(note, "%.md$", "")
    note_content = utils.read(note_path)
    return {name = note_name, content = note_content}
end

function get_lines(markdown_text)
    lines = {}
    
    for line in utils.match_all(markdown_text, "[^\r\n]+") do
        table.insert(lines, line)
    end

    return lines
end

-- Function to remove all instances of the link pattern
function remove_link(input_line, link)
    if input_line == nil or input_line == "" then
        return ""
    end
    link_pattern = "%[%[" .. link .. "%]%]"
    output_line = utils.replace(input_line, link_pattern, "")
    if utils.match(output_line, "^%s*$") != nil then
    	output_line = ""
    end
    return output_line
end

function extract_links(line, link_found)
    if link_found == nil then
        link_found = {}
    end
    processed_line = line

    for raw_link in utils.match_all(line, "%[%[(.-)%]%]") do
        parsed_links = parse_links_str(raw_link)
        for _, link in ipairs(parsed_links) do
            table.insert(link_found, link)
        end
        processed_line = remove_link(processed_line, raw_link)
    end

    return processed_line, link_found
end


function clean_content(content)
    if content == nil then
        return ""
    end
    cleaned_content = string.gsub(content, "'", "")
    cleaned_content = string.gsub(cleaned_content, '"', "")
    return cleaned_content
end

function process_content(content)
    content = unescape_string(content)
    content = clean_content(content)
    content_lines = get_lines(content)
    processed_lines = {}
    link_found = {}

    for _, line in ipairs(content_lines) do
        processed_line, updated_link_found = extract_links(line, link_found)
        link_found = updated_link_found
        if processed_line != "" then
        	table.insert(processed_lines, processed_line)
        end
    end

    processed_content = table.concat(processed_lines, "\n")
    return processed_content, link_found
end

-- A note's frontmatter, if any (task-tracked notes have one; plain
-- notes never have and never need one) -- see doc/unified-items-design.md.
-- `is_task` is the one field that matters for what this function does
-- besides store content; the rest just round-trip into tasks.
function is_task_frontmatter(metadata)
    return metadata != nil and metadata.is_task == "true"
end

function tasks_upsert_sql(item_id, metadata)
    esc_due_to = "NULL"
    if metadata.due_to != nil and metadata.due_to != "" then
        esc_due_to = "'" .. database.escape_sqlite(metadata.due_to) .. "'"
    end
    overdue_num = tonumber(metadata.overdue)
    if overdue_num == nil then
        overdue_num = 0
    end
    esc_done = "NULL"
    if metadata.done != nil and metadata.done != "" then
        esc_done = "'" .. database.escape_sqlite(metadata.done) .. "'"
    end
    esc_owner = "NULL"
    if metadata.owner != nil and metadata.owner != "" then
        esc_owner = "'" .. database.escape_sqlite(metadata.owner) .. "'"
    end
    importance_num = tonumber(metadata.importance)
    if importance_num == nil then
        importance_num = 1
    end
    urgency_num = tonumber(metadata.urgency)
    if urgency_num == nil then
        urgency_num = 1
    end

    return string.format("""
        INSERT INTO tasks (item_id, due_to, overdue, done, owner, importance, urgency)
        VALUES ('%s', %s, %d, %s, %s, %d, %d)
        ON CONFLICT(item_id) DO UPDATE SET
            due_to = excluded.due_to,
            overdue = excluded.overdue,
            done = excluded.done,
            owner = excluded.owner,
            importance = excluded.importance,
            urgency = excluded.urgency;
    """, item_id, esc_due_to, overdue_num, esc_done, esc_owner, importance_num, urgency_num)
end

function vault_to_sql(vault_path, brain_file)
    -- Ensure size/id columns exist (for upgrade path)
    pcall(database.sqlite_update, brain_file, "ALTER TABLE notes ADD COLUMN size INTEGER DEFAULT 0;") -- ignore error if already exists
    pcall(database.sqlite_update, brain_file, "ALTER TABLE notes ADD COLUMN id INTEGER;") -- ignore error if already exists
    pcall(database.sqlite_update, brain_file, require("sql_schema").sql_init) -- ensure tasks exists on old brains

    -- Load existing note metadata for incremental update
    existing_notes = {}
    query = "SELECT id, subject, title, time, size FROM notes;"
    rows_raw = database.sqlite_query(brain_file, query)
    rows = {}
    if rows_raw != nil then
        rows = rows_raw
    end
    for _, row in ipairs(rows) do
        -- Handle both named and numeric column access
        note_id = row[1]
        if row.id != nil then
            note_id = row.id
        end

        subject = ""
        if row[2] != nil then
            subject = row[2]
        end
        if row.subject != nil then
            subject = row.subject
        end

        title = ""
        if row[3] != nil then
            title = row[3]
        end
        if row.title != nil then
            title = row.title
        end

        time = ""
        if row[4] != nil then
            time = row[4]
        end
        if row.time != nil then
            time = row.time
        end

        size_field = 0
        if row[5] != nil then
            size_field = row[5]
        end
        if row.size != nil then
            size_field = row.size
        end
        size_num = tonumber(size_field)
        size = 0
        if size_num != nil then
            size = size_num
        end

        -- Keyed on the *sanitized* title (matching get_note_paths'
        -- "/" -> "-" filename substitution) so a title containing "/"
        -- still matches its own file on disk. The real, unsanitized
        -- title is kept in the map's value (not re-derivable from the
        -- key) for the WHERE clauses below, which must target the row
        -- by its actual stored title, not the sanitized filename text.
        key = subject .. "||" .. string.gsub(title, "/", "-")
        existing_notes[key] = {id = note_id, time = time, size = size, subject = subject, title = title}
    end

    vault_files = get_vault_files(vault_path)
    if vault_files == nil then
        print("Failed to read vault")
        return nil
    end

    sql_statements = {}
    seen_notes = {}
    updates_count = 0
    inserts_count = 0
    -- All inserts in this pass are batched into one transaction below,
    -- so generate_id's own DB uniqueness check can't see ids this same
    -- loop already picked but hasn't committed yet -- track them here
    -- too, or two new files in one resync can collide on the same id.
    -- On a collision, redraw with a bare math.random() call rather
    -- than calling generate_id again: generate_id reseeds math.random
    -- from os.time() (1s resolution) on every call, so retrying via
    -- generate_id within the same second would reseed to the same
    -- value and deterministically redraw the exact same "unique"
    -- candidate forever. A bare math.random() advances the
    -- already-seeded generator instead of resetting it.
    ids_used_this_pass = {}

    function next_note_id()
        candidate = bx_utils.generate_id("notes", nil, nil, brain_file)
        while ids_used_this_pass[candidate] != nil or bx_utils.is_id_unique("notes", candidate, brain_file) == false do
            candidate = string.format("%010d", math.random(10 ^ 9))
        end
        ids_used_this_pass[candidate] = true
        return candidate
    end

    for subject, notes in pairs(vault_files) do
        actual_subject = subject
        if subject == "root" then
            actual_subject = ""
        end

        for _, note_file in pairs(notes) do
            note_name = string.gsub(note_file, "%.md$", "")
            -- note_name/actual_subject come straight off the filesystem
            -- (a filename, a directory name) and can contain anything a
            -- title can -- including a plain English apostrophe, which
            -- is extremely common in free-text titles. Every SQL literal
            -- built from either below must go through this escape, or a
            -- title as ordinary as "job's integrity" breaks the
            -- statement (confirmed live: this exact bug deleted nothing
            -- only because a batched multi-statement transaction rolls
            -- back whole on a syntax error, not because it was harmless).
            esc_note_name = database.escape_sqlite(note_name)
            esc_actual_subject = database.escape_sqlite(actual_subject)
            note_key = actual_subject .. "||" .. note_name
            seen_notes[note_key] = true
            
            -- Resolve note file path
            note_path = nil
            if actual_subject != "" then
                note_path = joinpath(vault_path, actual_subject, note_file)
            else
                note_path = joinpath(vault_path, note_file)
            end

            last_update_time, file_size = get_file_attributes(note_path)
            
            existing = existing_notes[note_key]
            
            -- Absolute guarantee: update if time OR size differs
            needs_update = (existing == nil) or (existing.time != last_update_time) or (existing.size != file_size)

            if needs_update then
                raw_content = utils.read(note_path)
                -- Frontmatter is optional -- plain notes never have any,
                -- and parse_frontmatter is a no-op (empty metadata, body
                -- unchanged) when none is found, so this is safe for
                -- every existing note file. Only task-tracked items
                -- carry the extra keys tasks_upsert_sql needs.
                metadata, raw_body = bx_utils.parse_frontmatter(raw_content)
                content, links = process_content(raw_body)

                note_id = nil
                if existing != nil then
                    note_id = existing.id
                    if note_id == nil or note_id == "" then
                        note_id = next_note_id()
                    end
                    -- Target the row by its real stored (subject, title)
                    -- -- not the sanitized filename text, which only
                    -- matches when the title had no "/" to begin with.
                    esc_existing_subject = database.escape_sqlite(existing.subject)
                    esc_existing_title = database.escape_sqlite(existing.title)
                    -- Update existing note
                    update_note = string.format(
                        "UPDATE notes SET time='%s', size=%d, content='%s', id='%s' WHERE subject='%s' AND title='%s';",
                        last_update_time,
                        file_size,
                        content,
                        note_id,
                        esc_existing_subject,
                        esc_existing_title
                    )
                    table.insert(sql_statements, update_note)
                    -- Clear old connections
                    table.insert(sql_statements, string.format("DELETE FROM connections WHERE source_title='%s' AND source_subject='%s';", esc_existing_title, esc_existing_subject))
                    updates_count = updates_count + 1
                else
                    note_id = next_note_id()
                    -- Insert new note
                    insert_note = string.format(
                        "INSERT INTO notes (id, time, size, subject, title, content) VALUES ('%s', '%s', %d, '%s', '%s', '%s');",
                        note_id,
                        last_update_time,
                        file_size,
                        esc_actual_subject,
                        esc_note_name,
                        content
                    )
                    table.insert(sql_statements, insert_note)
                    inserts_count = inserts_count + 1
                end

                if is_task_frontmatter(metadata) then
                    table.insert(sql_statements, tasks_upsert_sql(note_id, metadata))
                end

                -- Insert connections if any
                if utils.length(links) > 0 then
                    insert_connections = "INSERT INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

                    for _, link in pairs(links) do
                        link_subject = ""
                        if link.subject != nil then
                            link_subject = link.subject
                        end
                        statement_value = string.format(
                            "('%s','%s','%s','%s'), ",
                            esc_note_name,
                            esc_actual_subject,
                            database.escape_sqlite(link.title),
                            database.escape_sqlite(link_subject)
                        )
                        insert_connections = insert_connections .. statement_value
                    end

                    -- Trim trailing comma and finalize
                    insert_connections = string.sub(insert_connections, 1, -3) .. ";"
                    table.insert(sql_statements, insert_connections)
                end
            end
        end
    end

    -- Delete notes that are no longer in the vault
    deletes_count = 0
    for note_key, existing_removed in pairs(existing_notes) do
        if seen_notes[note_key] == nil then
            -- Real (subject, title), not re-derived from the key --
            -- the key holds a sanitized title (see the load loop
            -- above), which would never match the row's actual stored
            -- title for anything that had a "/" in it.
            subject = existing_removed.subject
            title = existing_removed.title

            esc_subject = database.escape_sqlite(subject)
            esc_title = database.escape_sqlite(title)
            if existing_removed.id != nil and existing_removed.id != "" then
                table.insert(sql_statements, string.format("DELETE FROM tasks WHERE item_id='%s';", existing_removed.id))
            end
            table.insert(sql_statements, string.format("DELETE FROM notes WHERE subject='%s' AND title='%s';", esc_subject, esc_title))
            table.insert(sql_statements, string.format("DELETE FROM connections WHERE source_title='%s' AND source_subject='%s';", esc_title, esc_subject))
            deletes_count = deletes_count + 1
        end
    end

    if #sql_statements > 0 then
        table.insert(sql_statements, 1, "BEGIN TRANSACTION;")
        table.insert(sql_statements, "COMMIT;")
        transaction_sql = table.concat(sql_statements, "\n")
        status = database.sqlite_update(brain_file, transaction_sql)
        if status == nil then
            return nil
        end
    end
    
    return "success"
end

vault_update.parse_links_str = parse_links_str
vault_update.process_content = process_content
vault_update.vault_to_sql = vault_to_sql
vault_update.tasks_upsert_sql = tasks_upsert_sql
vault_update.is_task_frontmatter = is_task_frontmatter

-- Export the module
return vault_update
