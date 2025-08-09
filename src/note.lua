-- Define a module table
local note = {}

local lfs = require("lfs")
local get_vault_path = require("bx_utils").get_vault_path
local user = require("user")

local function proccess_links_str(links_str)
    links = split(links_str, ",")
    for idx,link in pairs(links) do
        links[idx] = strip(link)
    end
    return links
end
        
local function insert_note(brain_file, subject, title, content)
    local insert_statement = "INSERT INTO notes ('subject', 'name', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. content .. "');"
    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function append_content(brain_file, subject, title, content)
    local query = string.format("SELECT content FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note")
        return nil
    end
    local new_content = result[1].content .. "\n" .. content

    local update_statement = string.format("UPDATE notes SET content='%s' WHERE name='%s' AND subject='%s';", new_content, title, subject)

    local status = local_update(brain_file, update_statement)
    if not status then
        print("Failed to update database")
        return nil
    end
    return "success"
end

local function connect_notes(brain_file, source, links)
    local insert_statement = "INSERT OR IGNORE INTO connections (source, target) VALUES "
    for index, target in pairs(links) do
        local statement_value = "('" .. source .. "', '" .. target .. "'), "
        insert_statement = insert_statement .. statement_value
    end
    insert_statement = slice(insert_statement, 1, length(insert_statement)-2) .. ";"

    local status = local_update(brain_file, insert_statement)
    if not status then
        print("Failed to connect notes")
        return nil
    end
    return "success"
end

local function write_note(vault_dir, subject, title, content, links)
    local obsidian_links = {}
    for index, link in pairs(links) do
        table.insert(obsidian_links, "[[" .. link .. "]] ")
    end

    local note_dir = vault_dir .. "/" .. subject
    local note_path = note_dir .. "/" .. title .. ".md"

    -- Ensure the directory exists
    local mkdir_status = lfs.mkdir(note_dir)
    if not mkdir_status and not lfs.attributes(note_dir, "mode") then
        print("Could not create directory: " .. note_dir)
        return
    end
    
    local note_file = io.open(note_path, "a")
    if not note_file then
        print("Error: Could not open file: " .. note_path)
        return
    end

    local to_write = content .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    note_file:write(to_write)
    note_file:close()
    return "success"
end

local function take_note(brain_file, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    local content = args["content"] or ""
    local links_str = args["links"] or ""
    local links = proccess_links_str(links_str)

    local vault_dir = get_vault_path()

    if title == "" then
        print("Must provide note title")
        return
    end

    if content == "" then
        print("Must provide note content")
        return
    end

    local insert_status = insert_note(brain_file, subject, title, content)
    if not insert_status then
        print("Error: note insertion failed")
        return
    end
    
    if not isempty(links) then
        local connect_status = connect_notes(brain_file, title, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_dir then
        local write_status = write_note(vault_dir, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

local function edit_note(brain_file, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    local editor = get_default_editor()
    local vault_path = get_vault_path()

	if subject == "" and title == "" then
    	subject = "daily"
    	title = os.date("%Y-%m-%d")
	end
	
    local note_path = vault_path .. "/" .. subject .. "/" .. title .. ".md"
    if not lfs.attributes(note_path) then
        print("Note file does not exist: " .. note_path)
        return
    end

    local success = os.execute(string.format("'%s' '%s'", editor, note_path))
    if not success then
        print("Failed to open editor")
        return
    end

    success = update_note_from_file(brain_file, note_path)
    if not success then
        print("Failed to edit note in brain file")
        return
    end

    return "success"
end

local function last_notes(brain_file, args)
    local subject = args["subject"] or "daily"
    local num = args["number"] or 5

    local query = string.format("SELECT name, content FROM notes WHERE subject='%s' ORDER BY name DESC LIMIT %s", subject, num)

    local notes_empty = is_sqlite_empty(brain_file, "notes")
    if notes_empty then
        print("No notes available")
        return
    end

    local result = local_query(brain_file, query)
    if length(result) > 0 then
        -- view(result)
        for i, note in pairs(result) do
            bold(note.name)
            print(note.content .. "\n")
        end
    else
        print("No notes available")
    end
end

local function todays_note(brain_file, args)
    -- Get today's date in the format "YYYY-MM-DD"
    local title = os.date("%Y-%m-%d")
    local subject = "daily"
    local content = args["content"] or ""
    local links = args["links"] or {}

    local vault_dir = get_vault_path()

    if content == "" then
        print("Must provide note content")
    end

    -- Check if the note exists
    local query = string.format("SELECT COUNT(*) AS count FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = local_query(brain_file, query)
    if not result then
        print("Failed to query note database")
        return
    end

    local note_exists = tonumber(result[1].count) > 0

    -- If the note exists, update it; otherwise, create a new one
    if not isempty(content) then
        if note_exists then
            local append_status = append_content(brain_file, subject, title, content)
            if not append_status then
                print("Error: append content failed")
                return
            end
        else
            local insert_status = insert_note(brain_file, subject, title, content)
            if not insert_status then
                print("Error: note insertion failed")
                return
            end
        end
    end

    if not isempty(links) then
        local connect_status = connect_notes(brain_file, title, links)
        if not connect_status then
            print("Error: notes connection failed")
            return
        end
    end

    if vault_dir then
        local write_status = write_note(vault_dir, subject, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

local function get_help_string(subcommand)
    local help_strings = {
        ["brex note"] = [[
            Description:
            Creates or updates today's note with the specified content.
            Links can be provided as a comma-separated list.

            Required:
            -c --content <content> Note's content.
            
            Optional:
            -l --links <links> Links to other notes, separated by commas.
            
            Examples:
            brex note --content "This is today's note content"
            brex note --content "This is today's note content" --links "link1,link2"
        ]],
        ["brex note add"] = [[
            Description:
            Adds a new note with the specified title, and content.
            Links can be provided as a comma-separated list.

            Required:
            -t --title <title> Note's title.
            -c --content <content> Note's content.
            
            Optional:
            -s --subject <subject> Subject of the note.
            -l --links <links> Links to other notes, separated by commas.
            
            Examples:
            brex note add --title "My Note" --content "This is the content of my note"
            brex note add --title "My Note" --content "This is the content of my note" --subject "My Subject" --links "link1,link2"
        ]],
        ["brex note edit"] = [[
            Description:
            Opens the specified note in the default editor for editing.
            If the note does not exist, it will be created.

            Required:
            -t --title <title> Title of the note to edit.
            
            Optional:
            -s --subject <subject> Subject of the note.
            
            Examples:
            brex note edit --title "My Note"
            brex note edit --title "My Note" --subject "My Subject"
        ]],
        ["brex note last"] = [[
            Description:
            Displays the last notes.
            If no subject is provided, defaults to "daily". The number of notes displayed can be specified with the --number option, defaulting to 5.

            Optional:
            -s --subject <subject> Subject of the notes to display.
            -n --number <number> Number of notes to display, default is 5.
            
            Examples:
            brex note last
            brex note last --subject "daily" --number 10
        ]],
        ["brex note connect"] = [[
            Description:
            Connect notes.

            Required:
            -t --title <title> Title of the note to connect.
            -l --links <links> Links to other notes, separated by commas.
            
            Examples:
            brex note connect --title "note1" --links "note2,note3"
        ]]
    }

    return help_strings[subcommand]
end

local function do_note(brain_file)
    local arg_string = [[
        -d --do arg string false
        -s --subject arg string false
        -t --title arg string false
        -c --content arg string false
        -l --links arg string false
        -n --number arg number false
    ]]

    local help_string = get_help_string(arg[0])

    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    if args then
        if args["do"] == "add" then
            take_note(brain_file, args)
        elseif args["do"] == "edit" then
            edit_note(brain_file, args)
        elseif args["do"] == "last" then
            last_notes(brain_file, args)
        elseif args["do"] == "connect" then
            local links = proccess_links_str(args["links"])
            connect_notes(brain_file, args["title"], links)
        elseif not args["do"] then
            todays_note(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, edit, last")
        end
    end
end

note.do_note = do_note

if arg[0] == "note.lua" then
    do_note(brain_file)
else
    -- Export the module
    return note
end
