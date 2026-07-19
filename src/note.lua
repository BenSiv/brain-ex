-- Define a module table
note = {}

utils = require("utils")
argparse = require("argparse")
database = require("database")
local_update = database.local_update
local_query = database.local_query
config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
get_default_editor = config.get_default_editor
lfs = require("lfs")
parse_links_str = require("vault_to_sql").parse_links_str
help = require("help")
bx_utils = require("bx_utils")
prettyprint = require("prettyprint")

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function insert_note(brain_file, subject, title, content)
    subject = escape_sql(subject)
    title = escape_sql(title)
    content = escape_sql(content)
    insert_statement = "INSERT INTO notes ('subject', 'title', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. content .. "');"
    status = database.local_update(brain_file, insert_statement)
    if status == nil then
        return nil, "Failed to update database"
    end
    return true
end

function append_content(brain_file, subject, title, content)
    -- subject and title used in query must be escaped
    esc_subject = escape_sql(subject)
    esc_title = escape_sql(title)

    query = string.format("SELECT content FROM notes WHERE title='%s' AND subject='%s';", esc_title, esc_subject)
    result = database.local_query(brain_file, query)
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
    esc_content = escape_sql(new_content)

    update_statement = string.format("UPDATE notes SET content='%s' WHERE title='%s' AND subject='%s';", esc_content, esc_title, esc_subject)

    status = database.local_update(brain_file, update_statement)
    if status == nil then
        print("Failed to update database")
        return nil
    end
    return "success"
end

function connect_notes(brain_file, source_title, source_subject, links)
    if utils.isempty(links) == true then
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
            "('%s','%s','%s','%s'), ",
            source_title,
            safe_source_subject,
            target_title,
            target_subject
        )
        insert_statement = insert_statement .. statement_value
    end

    -- remove trailing comma + space and add semicolon
    insert_statement = string.sub(insert_statement, 1, -3) .. ";"

    status = database.local_update(brain_file, insert_statement)
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
    note_path = note_dir .. "/" .. title .. ".md"
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
    esc_subject = escape_sql(safe_subject)
    esc_title = escape_sql(safe_title)
    query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title='%s' AND subject='%s';", esc_title, esc_subject)
    result = database.local_query(brain_file, query)
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
        if vault_path != nil then
            status, err = write_note(vault_path, subject, title, content, links, "a")
            if status == nil then
                return nil, err
            end
            return sync_note_from_vault(brain_file, vault_path, subject, title)
        else
            status, err = append_content(brain_file, subject, title, content)
            if status == nil then
                 return nil, err
            end
        end
    else
        if vault_path != nil then
            if note_exists(brain_file, subject, title) then
                return nil, "Failed to update database"
            end
            status, err = write_note(vault_path, subject, title, content, links, "w")
            if status == nil then
                return nil, err
            end
            return sync_note_from_vault(brain_file, vault_path, subject, title)
        else
            status, err = insert_note(brain_file, subject, title, content)
            if status == nil then
                return nil, err
            end
        end
    end
    
    if utils.isempty(links) == false then
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
	    -- edit last log note?
    	-- subject = "log"
    	-- iso_local = os.date("%Y-%m-%d %H:%M:%S")
        -- title = replace(iso_local, " ", "_")
        return nil, "Must provide title of note to edit"
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

    query = string.format("SELECT title, content FROM notes WHERE subject='%s' ORDER BY title DESC LIMIT %s", subject, num)
    result = database.local_query(brain_file, query)

    if result != nil and utils.length(result) > 0 then
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
    esc_subject = escape_sql(subject)
    esc_title = escape_sql(title) -- title comes from os.date usually but good practice to escape if it ever changes
    query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title='%s' AND subject='%s';", esc_title, esc_subject)
    result = database.local_query(brain_file, query)
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
        if note_exists then
            write_mode = "a"
        end

        status, err = write_note(vault_path, subject, title, content, links, write_mode)
        if status == nil then
            return nil, err
        end
        return sync_note_from_vault(brain_file, vault_path, subject, title)
    end

    -- Insert or append content
    if utils.isempty(content) == false then
        if note_exists then
            status, err = append_content(brain_file, subject, title, content)
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

    if utils.isempty(links) == false and vault_path == nil then
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

    if utils.isempty(links) then
        return nil, "No links provided to connect."
    end

    -- Connect in the database
    vault_path = get_vault_path()
    if vault_path != nil then
        note_dir, note_path = get_note_paths(vault_path, subject, title)

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
            print("Failed to open note file: " .. note_path)
        end

        return sync_note_from_vault(brain_file, vault_path, subject, title)
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
    arg_string = """
        -d --do arg string false
        -s --subject arg string false
        -t --title arg string false
        -c --content arg string false
        -l --links arg string false
        -n --number arg number false
        -u --update flag boolean false
    """

    help_string = help.get_help_string(arg[0])
    expected_args = argparse.def_args(arg_string)
    args = argparse.parse_args(cmd_args, expected_args, help_string)
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

if string.match(arg[0], "note.lua$") != nil then
    do_note(get_brain_path(), arg)
else
    -- Export the module
    return note
end
