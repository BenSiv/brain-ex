-- Define a module table
note = {}

database = require("database")
db = database
local_update = database.exec
local_query = database.query
config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
get_default_editor = config.get_default_editor
lfs = require("lfs")
parse_links_str = require("vault_to_sql").parse_links_str
help = require("help")
bx_utils = require("bx_utils")
prettyprint = require("prettyprint")

const paths = require("paths")

function has_column(brain_file, table_name, col_name)
    cols = database.query(brain_file, "PRAGMA table_info(" .. table_name .. ");")
    if cols == nil then
        return false
    end
    for _, col in pairs(cols) do
        if col["name"] == col_name or col[2] == col_name then
            return true
        end
    end
    return false
end

function insert_note(brain_file, subject, title, content)
    body_text = content
    if body_text == nil then
        body_text = ""
    end
    insert_statement = nil
    if has_column(brain_file, "notes", "id") and has_column(brain_file, "notes", "size") then
        note_id = bx_utils.generate_id("notes", nil, nil, brain_file)
        insert_statement = string.format(
            "INSERT INTO notes ('id', 'subject', 'title', 'content', 'size') VALUES (%s, %s, %s, %s, %d);",
            db.quote(note_id), db.quote(subject), db.quote(title), db.quote(body_text), string.len(body_text)
        )
    else
        insert_statement = "INSERT INTO notes ('subject', 'title', 'content') VALUES (" .. db.quote(subject) .. ", " .. db.quote(title) .. ", " .. db.quote(body_text) .. ");"
    end
    status = database.exec(brain_file, insert_statement)
    if status == nil then
        return nil, "Failed to update database"
    end
    return true
end

-- The one shared "add a log entry" formatting: every real append to an
-- existing item's content (a comment on a task, a follow-up on a
-- plain note -- same operation either way) goes through this, so a
-- task's comment log and a plain note's appended history look
-- identical. Not used for an item's initial content -- that's the
-- body, not a log entry appended to it.
function timestamped_entry(content)
    return os.date("%Y-%m-%d %H:%M:%S") .. "\n" .. content
end

function append_content(brain_file, subject, title, content)
    query = string.format("SELECT content FROM notes WHERE title=%s AND subject=%s;", db.quote(title), db.quote(subject))
    result = database.query(brain_file, query)
    if result == nil or #result == 0 then
        return nil, "Failed to find note for append: " .. title
    end
    -- Handle both named and numeric column access
    old_content = ""
    if result[1][1] != nil then
        old_content = result[1][1]
    end
    if result[1].content != nil then
        old_content = result[1].content
    end
    new_content = old_content .. "\n" .. content

    update_statement = nil
    if has_column(brain_file, "notes", "size") then
        update_statement = string.format(
            "UPDATE notes SET content=%s, size=%d, time=datetime('now', 'localtime') WHERE title=%s AND subject=%s;",
            db.quote(new_content), string.len(new_content), db.quote(title), db.quote(subject)
        )
    else
        update_statement = string.format("UPDATE notes SET content=%s WHERE title=%s AND subject=%s;", db.quote(new_content), db.quote(title), db.quote(subject))
    end

    status = database.exec(brain_file, update_statement)
    if status == nil then
        print("Failed to update database")
        return nil
    end
    return "success"
end

function connect_notes(brain_file, source_title, source_subject, links)
    if bx_utils.isempty(links) == true then
        return true
    end

    insert_statement = "INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

    safe_source_subject = ""
    if source_subject != nil then
        safe_source_subject = source_subject
    end

    for _, link in pairs(links) do
        target_title = link.title
        target_subject = ""
        if link.subject != nil then
            target_subject = link.subject
        end

        statement_value = string.format(
            "(%s,%s,%s,%s), ",
            db.quote(source_title),
            db.quote(safe_source_subject),
            db.quote(target_title),
            db.quote(target_subject)
        )
        insert_statement = insert_statement .. statement_value
    end

    -- remove trailing comma + space and add semicolon
    insert_statement = string.sub(insert_statement, 1, -3) .. ";"

    status = database.exec(brain_file, insert_statement)
    if status == nil then
        return nil, "Failed to connect notes"
    end
    return true
end

function get_note_paths(vault_dir, subject, title)
    note_dir = vault_dir
    if subject != nil and subject != "" then
        note_dir = vault_dir .. "/" .. subject
    end
    -- A title can be arbitrary free text (this matters for migrated
    -- tasks especially, whose titles started life as prose, not a
    -- filename); "/" would otherwise be read as a path separator and
    -- silently create/require an extra directory level.
    safe_title = string.gsub(title, "/", "-")
    note_path = note_dir .. "/" .. safe_title .. ".md"
    return note_dir, note_path
end

function note_exists(brain_file, subject, title)
    safe_subject = ""
    if subject != nil then
        safe_subject = subject
    end
    safe_title = ""
    if title != nil then
        safe_title = title
    end
    query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title=%s AND subject=%s;", db.quote(safe_title), db.quote(safe_subject))
    result = database.query(brain_file, query)
    if result == nil or result[1] == nil then
        return false
    end
    count_val = 0
    if result[1][1] != nil then
        count_val = result[1][1]
    end
    if result[1].count != nil then
        count_val = result[1].count
    end
    return tonumber(count_val) > 0
end

function sync_note_from_vault(brain_file, vault_dir, subject, title)
    _, note_path = get_note_paths(vault_dir, subject, title)
    return bx_utils.update_note_from_file(brain_file, note_path)
end

function write_note(vault_dir, subject, title, content, links, mode)
    obsidian_links = {}
    for _, link in pairs(links) do
        -- each link is a table {title=..., subject=...}
        link_path = nil
        if link.subject != nil and link.subject != "" then
            link_path = link.subject .. "/" .. link.title
        else
            link_path = link.title
        end
        table.insert(obsidian_links, "[[" .. link_path .. "]]")
    end

    note_dir, note_path = get_note_paths(vault_dir, subject, title)
    if mode == nil then
        mode = "a"
    end

    -- Ensure the directory exists
    if lfs.attributes(note_dir, "mode") == nil then
        mkdir_status = lfs.mkdir(note_dir)
        if mkdir_status != true and lfs.attributes(note_dir, "mode") == nil then
            return nil, "Could not create directory: " .. note_dir
        end
    end

    note_file = io.open(note_path, mode)
    if note_file == nil then
        return nil, "Error: Could not open file: " .. note_path
    end

    to_write = content .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    io.write(note_file, to_write)
    io.close(note_file)
    return true
end

-- Writes the vault file, then syncs the database from it. The file is
-- the source of truth, so a failed sync would otherwise leave it
-- changed while the database isn't -- and every retry of the same
-- command would append the entry again. On failure the file is put
-- back as it was (or removed, if this call created it) and re-synced,
-- so the database matches it again too.
function write_and_sync_note(brain_file, vault_dir, subject, title, content, links, mode)
    _, note_path = get_note_paths(vault_dir, subject, title)
    previous = nil
    existing_file = io.open(note_path, "r")
    if existing_file != nil then
        previous = io.read(existing_file, "*all")
        io.close(existing_file)
    end

    status, err = write_note(vault_dir, subject, title, content, links, mode)
    if status == nil then
        return nil, err
    end
    status, err = sync_note_from_vault(brain_file, vault_dir, subject, title)
    if status != nil then
        return status
    end

    if previous == nil then
        os.remove(note_path)
    else
        restore_file = io.open(note_path, "w")
        if restore_file != nil then
            io.write(restore_file, previous)
            io.close(restore_file)
            sync_note_from_vault(brain_file, vault_dir, subject, title)
        end
    end
    if err == nil then
        err = "Failed to update database"
    end
    return nil, err .. " (note file left unchanged)"
end

function take_note(brain_file, args)
    subject = ""
    if args["subject"] != nil then
        subject = args["subject"]
    end
    title = ""
    if args["title"] != nil then
        title = args["title"]
    end
    content = ""
    if args["content"] != nil then
        content = args["content"]
    end
    links_str = ""
    if args["links"] != nil then
        links_str = args["links"]
    end
    links = parse_links_str(links_str)

    vault_path = get_vault_path()

    if title == "" then
        return nil, "Must provide note title"
    end

    if content == "" then
        return nil, "Must provide note content"
    end

    if args["update"] == true then
        entry = timestamped_entry(content)
        if vault_path != nil then
            return write_and_sync_note(brain_file, vault_path, subject, title, entry, links, "a")
        else
            status, err = append_content(brain_file, subject, title, entry)
            if status == nil then
                 return nil, err
            end
        end
    else
        if vault_path != nil then
            if note_exists(brain_file, subject, title) then
                return nil, "Failed to update database"
            end
            return write_and_sync_note(brain_file, vault_path, subject, title, content, links, "w")
        else
            status, err = insert_note(brain_file, subject, title, content)
            if status == nil then
                return nil, err
            end
        end
    end
    
    if bx_utils.isempty(links) == false then
        status, err = connect_notes(brain_file, title, subject, links)
        if status == nil then
            return nil, err
        end
    end
    return true
end

function edit_note(brain_file, args)
    subject = ""
    if args["subject"] != nil then
        subject = args["subject"]
    end
    title = ""
    if args["title"] != nil then
        title = args["title"]
    end
    editor = get_default_editor()
    vault_path = get_vault_path()

	if title == "" then
        return nil, "Must provide title of note to edit"
	end

    if vault_path == nil or vault_path == "" then
        if editor == nil or editor == "" then
            editor = "nano"
        end
        existing_rows = database.query(
            brain_file,
            string.format("SELECT content FROM notes WHERE title=%s AND subject=%s;", db.quote(title), db.quote(subject))
        )
        initial_content = ""
        existed = false
        if existing_rows != nil and #existing_rows > 0 then
            existed = true
            if existing_rows[1][1] != nil then
                initial_content = existing_rows[1][1]
            end
            if existing_rows[1].content != nil then
                initial_content = existing_rows[1].content
            end
        end

        tmp_path = os.tmpname() .. ".md"
        if not paths.write_file(tmp_path, initial_content) then
            return nil, "Could not create temporary file: " .. tmp_path
        end

        edit_ok = os.execute(string.format("'%s' '%s'", editor, tmp_path))
        if edit_ok == nil or edit_ok == false then
            os.remove(tmp_path)
            return nil, "Failed to open editor"
        end

        edited_body = paths.read_file(tmp_path)
        os.remove(tmp_path)
        if edited_body == nil then
            edited_body = ""
        end

        if existed then
            upd_sql = nil
            if has_column(brain_file, "notes", "size") then
                upd_sql = string.format(
                    "UPDATE notes SET content=%s, size=%d, time=datetime('now', 'localtime') WHERE title=%s AND subject=%s;",
                    db.quote(edited_body), string.len(edited_body), db.quote(title), db.quote(subject)
                )
            else
                upd_sql = string.format(
                    "UPDATE notes SET content=%s WHERE title=%s AND subject=%s;",
                    db.quote(edited_body), db.quote(title), db.quote(subject)
                )
            end
            if database.exec(brain_file, upd_sql) == nil then
                return nil, "Failed to edit note in brain file"
            end
        else
            ok_ins, ins_err = insert_note(brain_file, subject, title, edited_body)
            if ok_ins == nil then
                return nil, ins_err
            end
        end

        database.exec(
            brain_file,
            string.format("DELETE FROM connections WHERE source_title=%s AND source_subject=%s;", db.quote(title), db.quote(subject))
        )
        extracted_links = {}
        for raw_link in string.gmatch(edited_body, "%[%[([^%[%]\n]+)%]%]") do
            trimmed_link = string.trim(raw_link)
            if trimmed_link != "" then
                slash_idx = string.find(trimmed_link, "/", 1, true)
                if slash_idx != nil then
                    table.insert(extracted_links, {
                        subject = string.sub(trimmed_link, 1, slash_idx - 1),
                        title = string.sub(trimmed_link, slash_idx + 1)
                    })
                else
                    table.insert(extracted_links, {subject = "", title = trimmed_link})
                end
            end
        end
        if #extracted_links > 0 then
            connect_notes(brain_file, title, subject, extracted_links)
        end
        return true
    end
	
    _, note_path = get_note_paths(vault_path, subject, title)
    
    -- Create the file if it doesn't exist
    if lfs.attributes(note_path) == nil then
        note_dir, _ = get_note_paths(vault_path, subject, title)
        if lfs.attributes(note_dir, "mode") == nil then
            mkdir_status = lfs.mkdir(note_dir)
            if mkdir_status != true and lfs.attributes(note_dir, "mode") == nil then
                return nil, "Could not create directory: " .. note_dir
            end
        end
        -- Create an empty file
        file = io.open(note_path, "w")
        if file != nil then
            io.close(file)
        else
            return nil, "Could not create file: " .. note_path
        end
    end

    success = os.execute(string.format("'%s' '%s'", editor, note_path))
    if success == nil then
        return nil, "Failed to open editor"
    end

    success = bx_utils.update_note_from_file(brain_file, note_path)
    if success == nil then
        return nil, "Failed to edit note in brain file"
    end

    return true
end

function last_notes(brain_file, args)
    subject = "log"
    if args["subject"] != nil then
        subject = args["subject"]
    end
    num = 5
    if args["number"] != nil then
        num = args["number"]
    end

    num_val = tonumber(num)
    if num_val == nil then
        num_val = 5
    end
    query = string.format("SELECT title, content FROM notes WHERE subject=%s ORDER BY title DESC LIMIT %d", db.quote(subject), num_val)
    result = database.query(brain_file, query)

    if result != nil and bx_utils.length(result) > 0 then
        for i, note in pairs(result) do
            -- Handle both named and numeric column access
            note_title = ""
            if note[1] != nil then
                note_title = note[1]
            end
            if note.title != nil then
                note_title = note.title
            end
            note_content = ""
            if note[2] != nil then
                note_content = note[2]
            end
            if note.content != nil then
                note_content = note.content
            end
            prettyprint.bold(note_title)
            print(note_content .. "\n")
        end
    else
        print("No notes")
    end
    return true
end

function log_note(brain_file, args)
    title = os.date("%Y-%m-%d_%H:%M:%S")
    subject = "log"
    if args["subject"] != nil then
        subject = args["subject"]
    end
    content = ""
    if args["content"] != nil then
        content = args["content"]
    end
    links_str = ""
    if args["links"] != nil then
        links_str = args["links"]
    end
    links = parse_links_str(links_str)

    vault_path = get_vault_path()

    if content == "" then
        return nil, "Must provide note content"
    end

    -- Check if the note exists
    query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title=%s AND subject=%s;", db.quote(title), db.quote(subject))
    result = database.query(brain_file, query)
    if result == nil then
        return nil, "Failed to query note database"
    end

    count_val = nil
    if result[1][1] != nil then
        count_val = result[1][1]
    end
    if result[1].count != nil then
        count_val = result[1].count
    end
    note_exists = tonumber(count_val) > 0

    if vault_path != nil then
        write_mode = "w"
        write_content = content
        if note_exists then
            write_mode = "a"
            write_content = timestamped_entry(content)
        end

        return write_and_sync_note(brain_file, vault_path, subject, title, write_content, links, write_mode)
    end

    -- Insert or append content
    if bx_utils.isempty(content) == false then
        if note_exists then
            status, err = append_content(brain_file, subject, title, timestamped_entry(content))
            if status == nil then
                return nil, err
            end
        else
            status, err = insert_note(brain_file, subject, title, content)
            if status == nil then
                return nil, err
            end
        end
    end

    if bx_utils.isempty(links) == false and vault_path == nil then
        status, err = connect_notes(brain_file, title, subject, links)
        if status == nil then
            return nil, err
        end
    end

    return true
end

function do_note_connect(brain_file, args)
    title = os.date("%Y-%m-%d_%H:%M:%S")
    if args["title"] != nil then
        title = args["title"]
    end
    subject = "log"
    if args["subject"] != nil then
        subject = args["subject"]
    end
    links_str = ""
    if args["links"] != nil then
        links_str = args["links"]
    end
    links = parse_links_str(links_str)

    if bx_utils.isempty(links) then
        return nil, "No links provided to connect."
    end

    -- Connect in the database
    vault_path = get_vault_path()
    if vault_path != nil then
        note_dir, note_path = get_note_paths(vault_path, subject, title)

        previous = nil
        existing_file = io.open(note_path, "r")
        if existing_file != nil then
            previous = io.read(existing_file, "*all")
            io.close(existing_file)
        end

        -- Ensure directory exists
        if lfs.attributes(note_dir, "mode") == nil then
            lfs.mkdir(note_dir)
        end

        note_file = io.open(note_path, "a")
        if note_file != nil then
            obsidian_links = {}
            for _, link in pairs(links) do
                if link.subject != "" then
                    table.insert(obsidian_links, "[[" .. link.subject .. "/" .. link.title .. "]]")
                else
                    table.insert(obsidian_links, "[[" .. link.title .. "]]")
                end
            end
            io.write(note_file, table.concat(obsidian_links, " ") .. "\n")
            io.close(note_file)
        else
            return nil, "Failed to open note file: " .. note_path
        end

        status, err = sync_note_from_vault(brain_file, vault_path, subject, title)
        if status != nil then
            return status
        end

        if previous == nil then
            os.remove(note_path)
        else
            restore_file = io.open(note_path, "w")
            if restore_file != nil then
                io.write(restore_file, previous)
                io.close(restore_file)
                sync_note_from_vault(brain_file, vault_path, subject, title)
            end
        end
        if err == nil then
            err = "Failed to update database"
        end
        return nil, err .. " (note file left unchanged)"
    end

    status, err = connect_notes(brain_file, title, subject, links)
    if status == nil then
        return nil, err
    end

    return true
end

function do_note(brain_file, cmd_args)
    subcommand = cmd_args[1]
    if subcommand != nil and string.sub(subcommand, 1, 1) != "-" then
        valid_subs = {
            ["add"] = true,
            ["edit"] = true,
            ["last"] = true,
            ["connect"] = true
        }
        if valid_subs[subcommand] == nil then
            print("Unknown subcommand: " .. subcommand)
            print("Available subcommands: add, edit, last, connect")
            return "success"
        end
        table.insert(cmd_args, 1, "-d")
    else
        subcommand = nil
    end
    flag_specs = {
        {short = "d", long = "do", kind = "string"},
        {short = "s", long = "subject", kind = "string"},
        {short = "t", long = "title", kind = "string"},
        {short = "c", long = "content", kind = "string"},
        {short = "l", long = "links", kind = "string"},
        {short = "n", long = "number", kind = "number"},
        {short = "u", long = "update", kind = "boolean"},
    }

    help_string = help.get_help_string(arg[0])
    args = bx_utils.parse_cmd_args(cmd_args, flag_specs, help_string)
    if args == nil then
        return "success"
    end
    
    status, err = nil, nil
    if args != nil then
        if args["do"] == "add" then
            status, err = take_note(brain_file, args)
        elseif args["do"] == "edit" then
            status, err = edit_note(brain_file, args)
        elseif args["do"] == "last" then
            status, err = last_notes(brain_file, args)
        elseif args["do"] == "connect" then
            status, err = do_note_connect(brain_file, args)
        elseif args["do"] == nil then
            status, err = log_note(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, edit, last")
            return "success" -- Help printed
        end
    end
    if status != true then
        err_msg = "Note command failed"
        if err != nil then
            err_msg = err
        end
        print(err_msg)
        return "error"
    end
    return "success"
end

note.do_note = do_note
note.take_note = take_note
note.log_note = log_note
note.edit_note = edit_note
note.last_notes = last_notes
note.do_note_connect = do_note_connect
note.append_content = append_content
note.timestamped_entry = timestamped_entry
note.get_note_paths = get_note_paths
note.note_exists = note_exists

if string.match(arg[0], "note.lua$") != nil then
    do_note(get_brain_path(), arg)
else
    -- Export the module
    return note
end
