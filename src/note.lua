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
update = require("update")

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function insert_note(brain_file, subject, title, content)
    subject = escape_sql(subject)
    title = escape_sql(title)
    content = escape_sql(content)
    insert_statement = "INSERT INTO notes ('subject', 'title', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. content .. "');"
    status = local_update(brain_file, insert_statement)
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
    result = local_query(brain_file, query)
    if result == nil or #result == 0 then
        return nil, "Failed to find note for append: " .. title
    end
    -- Handle both named and numeric column access
    old_content = result[1].content or result[1][1] or ""
    new_content = old_content .. "\n" .. content
    esc_content = escape_sql(new_content)

    update_statement = string.format("UPDATE notes SET content='%s' WHERE title='%s' AND subject='%s';", esc_content, esc_title, esc_subject)

    status = local_update(brain_file, update_statement)
    if status == nil then
        print("Failed to update database")
        return nil
    end
    return "success"
end

function connect_notes(brain_file, source_title, source_subject, links)
    if isempty(links) == true then
        return true
    end

    insert_statement = "INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES "

    for _, link in pairs(links) do
        target_title = link.title
        target_subject = link.subject or ""

        statement_value = string.format(
            "('%s','%s','%s','%s'), ",
            source_title,
            source_subject or "",
            target_title,
            target_subject
        )
        insert_statement = insert_statement .. statement_value
    end

    -- remove trailing comma + space and add semicolon
    insert_statement = string.sub(insert_statement, 1, -3) .. ";"

    status = local_update(brain_file, insert_statement)
    if status == nil then
        return nil, "Failed to connect notes"
    end
    return true
end

function write_note(vault_dir, subject, title, content, links)
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

    note_dir = vault_dir .. "/" .. subject
    note_path = note_dir .. "/" .. title .. ".md"

    -- Ensure the directory exists
    mkdir_status = lfs.mkdir(note_dir)
    if mkdir_status != true and lfs.attributes(note_dir, "mode") == nil then
        return nil, "Could not create directory: " .. note_dir
    end

    note_file = io.open(note_path, "a")
    if note_file == nil then
        return nil, "Error: Could not open file: " .. note_path
    end

    to_write = content .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    io.write(note_file, to_write)
    io.close(note_file)
    return true
end

function take_note(brain_file, args)
    subject = args["subject"] or ""
    title = args["title"] or ""
    content = args["content"] or ""
    links_str = args["links"] or ""
    links = parse_links_str(links_str)

    vault_path = get_vault_path()

    if title == "" then
        return nil, "Must provide note title"
    end

    if content == "" then
        return nil, "Must provide note content"
    end

    if args["update"] == true then
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
    
    if isempty(links) == false then
        status, err = connect_notes(brain_file, title, subject, links)
        if status == nil then
            return nil, err
        end
    end

    if vault_path != nil then
        status, err = write_note(vault_path, subject, title, content, links)
        if status == nil then
            return nil, err
        end
    end

    return true
end

function edit_note(brain_file, args)
    subject = args["subject"] or ""
    title = args["title"] or ""
    editor = get_default_editor()
    vault_path = get_vault_path()

	if title == "" then
	    -- edit last log note?
    	-- subject = "log"
    	-- iso_local = os.date("%Y-%m-%d %H:%M:%S")
        -- title = replace(iso_local, " ", "_")
        return nil, "Must provide title of note to edit"
	end
	
    note_path = vault_path .. "/" .. subject .. "/" .. title .. ".md"
    
    -- Create the file if it doesn't exist
    if lfs.attributes(note_path) == nil then
        note_dir = vault_path .. "/" .. subject
        mkdir_status = lfs.mkdir(note_dir)
        if mkdir_status != true and lfs.attributes(note_dir, "mode") == nil then
            return nil, "Could not create directory: " .. note_dir
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

    success = update.update_note_from_file(brain_file, note_path)
    if success == nil then
        return nil, "Failed to edit note in brain file"
    end

    return true
end

function last_notes(brain_file, args)
    subject = args["subject"] or "log"
    num = args["number"] or 5

    query = string.format("SELECT title, content FROM notes WHERE subject='%s' ORDER BY title DESC LIMIT %s", subject, num)
    result = local_query(brain_file, query)

    if result != nil and length(result) > 0 then
        for i, note in pairs(result) do
            -- Handle both named and numeric column access
            note_title = note.title or note[1] or ""
            note_content = note.content or note[2] or ""
            bold(note_title)
            print(note_content .. "\n")
        end
    else
        print("No notes")
    end
    return true
end

function log_note(brain_file, args)
    title = os.date("%Y-%m-%d_%H:%M:%S")
    subject = args["subject"] or "log"
    content = args["content"] or ""
    links_str = args["links"] or ""
    links = parse_links_str(links_str)

    vault_path = get_vault_path()

    if content == "" then
        return nil, "Must provide note content"
    end

    -- Check if the note exists
    esc_subject = escape_sql(subject)
    esc_title = escape_sql(title) -- title comes from os.date usually but good practice to escape if it ever changes
    query = string.format("SELECT COUNT(*) AS count FROM notes WHERE title='%s' AND subject='%s';", esc_title, esc_subject)
    result = local_query(brain_file, query)
    if result == nil then
        return nil, "Failed to query note database"
    end

    count_val = result[1].count or result[1][1]
    note_exists = tonumber(count_val) > 0

    -- Insert or append content
    if isempty(content) == false then
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

    if isempty(links) == false then
        status, err = connect_notes(brain_file, title, subject, links)
        if status == nil then
            return nil, err
        end
    end

    if vault_path != nil then
        status, err = write_note(vault_path, subject, title, content, links)
        if status == nil then
            return nil, err
        end
    end

    return true
end

function do_note_connect(brain_file, args)
    title = args["title"] or os.date("%Y-%m-%d_%H:%M:%S")
    subject = args["subject"] or "log"
    links_str = args["links"] or ""
    links = parse_links_str(links_str)

    if isempty(links) then
        return nil, "No links provided to connect."
    end

    -- Connect in the database
    status, err = connect_notes(brain_file, title, subject, links)
    if status == nil then
        return nil, err
    end

    -- Also append links to the note file
    vault_path = get_vault_path()
    if vault_path != nil then
        note_dir = vault_path .. "/" .. subject
        note_path = note_dir .. "/" .. title .. ".md"

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
    end

    return true
end

function do_note(brain_file, cmd_args)
    if cmd_args[1] != nil and string.sub(cmd_args[1], 1, 1) != "-" then
        table.insert(cmd_args, 1, "-d")
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

    help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)
    
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
        print(err or "Note command failed")
        return "error"
    end
    return "success"
end

note.do_note = do_note

if string.match(arg[0], "note.lua$") != nil then
    do_note(get_brain_path(), arg)
else
    -- Export the module
    return note
end
