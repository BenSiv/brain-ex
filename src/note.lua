-- Define a module table
local note = {}

local lfs = require("lfs")
local command_utils = require("command_utils")
local get_help_string = require("help").get_help_string
local bx_utils = require("bx_utils")

local function proccess_links_str(links_str)
    if not links_str or links_str == "" then
        return {}
    end
    links = split(links_str, ",")
    for idx,link in pairs(links) do
        links[idx] = strip(link)
    end
    return links
end

local function insert_note(state, subject, title, content)
    local insert_statement = "INSERT INTO notes ('subject', 'name', 'content') VALUES ('" .. subject .. "', '" .. title .. "', '" .. content .. "');"
    local success = state.database.execute(state.brain_file, insert_statement)
    if not success then
        return false, "Failed to update database"
    end
    return true
end

local function append_content(state, subject, title, content)
    local query = string.format("SELECT content FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = state.database.query(state.brain_file, query)
    if not result then
        return false, "Failed to query note"
    end
    local new_content = result[1].content .. "\n" .. content

    local update_statement = string.format("UPDATE notes SET content='%s' WHERE name='%s' AND subject='%s';", new_content, title, subject)

    local success = state.database.execute(state.brain_file, update_statement)
    if not success then
        return false, "Failed to update database"
    end
    return true
end

local function connect_notes(state, source, links)
    if isempty(links) then
        return true
    end
    
    local insert_statement = "INSERT OR IGNORE INTO connections (source, target) VALUES "
    for index, target in pairs(links) do
        local statement_value = "('" .. source .. "', '" .. target .. "'), "
        insert_statement = insert_statement .. statement_value
    end
    insert_statement = slice(insert_statement, 1, length(insert_statement)-2) .. ";"

    local success = state.database.execute(state.brain_file, insert_statement)
    if not success then
        return false, "Failed to connect notes"
    end
    return true
end

local function write_note(state, subject, title, content, links)
    if not state.vault_path then
        return true -- No vault configured, skip file writing
    end
    
    local obsidian_links = {}
    for index, link in pairs(links) do
        table.insert(obsidian_links, "[[" .. link .. "]] ")
    end

    local note_dir = state.vault_path .. "/" .. subject
    local note_path = note_dir .. "/" .. title .. ".md"

    -- Ensure the directory exists
    local mkdir_status = lfs.mkdir(note_dir)
    if not mkdir_status and not lfs.attributes(note_dir, "mode") then
        return false, "Could not create directory: " .. note_dir
    end

    local note_file = io.open(note_path, "a")
    if not note_file then
        return false, "Could not open file: " .. note_path
    end

    local to_write = content .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    note_file:write(to_write)
    note_file:close()
    return true
end
local function take_note(state, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    local content = args["content"] or ""
    local links_str = args["links"] or ""
    local links = proccess_links_str(links_str)

    if title == "" then
        return false, "Must provide note title"
    end

    if content == "" then
        return false, "Must provide note content"
    end

    local success, err = insert_note(state, subject, title, content)
    if not success then
        return false, err
    end

    success, err = connect_notes(state, title, links)
    if not success then
        return false, err
    end

    success, err = write_note(state, subject, title, content, links)
    if not success then
        return false, err
    end

    return true
end

local function edit_note(state, args)
    local subject = args["subject"] or ""
    local title = args["title"] or ""
    
    if subject == "" and title == "" then
        subject = "daily"
        title = os.date("%Y-%m-%d")
    end
    
    if not state.vault_path then
        return false, "No vault configured"
    end
    
    local note_path = state.vault_path .. "/" .. subject .. "/" .. title .. ".md"
    if not lfs.attributes(note_path) then
        return false, "Note file does not exist: " .. note_path
    end

    local success = os.execute(string.format("'%s' '%s'", state.default_editor, note_path))
    if not success then
        return false, "Failed to open editor"
    end

    -- Import update function
    local update = require("update")
    success, err = update.update_note_from_file(state, note_path)
    if not success then
        return false, err or "Failed to edit note in brain file"
    end

    return true
end
local function last_notes(state, args)
    local subject = args["subject"] or "daily"
    local num = args["number"] or 5

    local query = string.format("SELECT name, content FROM notes WHERE subject='%s' ORDER BY name DESC LIMIT %s", subject, num)

    local notes_empty = bx_utils.is_sqlite_empty(state, "notes")
    if notes_empty then
        return false, "No notes available"
    end

    local result = state.database.query(state.brain_file, query)
    if length(result) > 0 then
        for i, note in pairs(result) do
            bold(note.name)
            print(note.content .. "\n")
        end
    else
        return false, "No notes available"
    end
    
    return true
end

local function todays_note(state, args)
    -- Get today's date in the format "YYYY-MM-DD"
    local title = os.date("%Y-%m-%d")
    local subject = "daily"
    local content = args["content"] or ""
    local links_str = args["links"] or ""
    local links = proccess_links_str(links_str)

    if content == "" then
        return false, "Must provide note content"
    end

    -- Check if the note exists
    local query = string.format("SELECT COUNT(*) AS count FROM notes WHERE name='%s' AND subject='%s';", title, subject)
    local result = state.database.query(state.brain_file, query)
    if not result then
        return false, "Failed to query note database"
    end

    local note_exists = tonumber(result[1].count) > 0

    -- If the note exists, update it; otherwise, create a new one
    local success, err
    if note_exists then
        success, err = append_content(state, subject, title, content)
        if not success then
            return false, err
        end
    else
        success, err = insert_note(state, subject, title, content)
        if not success then
            return false, err
        end
    end

    success, err = connect_notes(state, title, links)
    if not success then
        return false, err
    end

    success, err = write_note(state, subject, title, content, links)
    if not success then
        return false, err
    end

    return true
end
function note.do_note(state)
    local arg_string = [[
        -d --do arg string false
        -s --subject arg string false
        -t --title arg string false
        -c --content arg string false
        -l --links arg string false
        -n --number arg number false
    ]]

    local help_string = get_help_string(arg[0])
    local args = command_utils.parse_command_args(arg_string, help_string)

    if not args then
        return false
    end

    local success, err

    if args["do"] == "add" then
        success, err = take_note(state, args)
    elseif args["do"] == "edit" then
        success, err = edit_note(state, args)
    elseif args["do"] == "last" then
        success, err = last_notes(state, args)
    elseif args["do"] == "connect" then
        local links = proccess_links_str(args["links"])
        success, err = connect_notes(state, args["title"], links)
    elseif not args["do"] then
        success, err = todays_note(state, args)
    else
        success, err = false, "Unknown subcommand: " .. args["do"]
    end

    if not success then
        command_utils.handle_command_error(err or "Unknown error", help_string)
        return false
    end

    return true
end

-- Export the module
return note
