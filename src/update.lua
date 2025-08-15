-- Define a module table
local update = {}

local vault_to_sql = require("vault_to_sql").vault_to_sql
local process_content = require("vault_to_sql").process_content
local sql_init = require("init").sql_init
local command_utils = require("command_utils")
local get_help_string = require("help").get_help_string

function update_from_vault(state)
    local task_file = joinpath(state.vault_path, "tasks.tsv")

    if state.brain_file and state.vault_path then
        os.remove(state.brain_file)

        -- create database and tables
        local success = state.database.execute(state.brain_file, sql_init)
        if not success then
            return false, "Failed to update database"
        end

        success = vault_to_sql(state.vault_path, state.brain_file)
        if not success then
            return false, "Failed to update from vault"
        end
    end

    if file_exists(task_file) then
        local success = import_delimited(state.brain_file, task_file, "tasks", "\t")
        if not success then
            return false, "Failed to import tasks"
        end
    end
    return true
end

function update.update_note_from_file(state, note_path)
    note_path = note_path or user.input("Note path: ")

    local title = "note"
    local subject = ""

    if state.vault_path then
        -- Extract subject and title from the note path
        title = note_path:match("([^/]+)%.md$")
        subject = note_path:match(".*/([^/]+)/[^/]+%.md$")
    else
        title = user.input("Title: ")
        subject = user.input("Subject: ")
    end

    -- Read content from the note file
    local content = read(note_path)
    if not content then
        return false, "Failed to read note: " .. note_path
    end

    content, links = process_content(content)

    -- Escape single quotes for SQL
    content = content:gsub("'", "''")

    -- Check if the note already exists
    local note_exists_query = string.format([[
        SELECT COUNT(*) AS num FROM notes
        WHERE subject = '%s' AND name = '%s'
    ]], subject, title)
    
    local result = state.database.query(state.brain_file, note_exists_query)
    if not result then
        return false, "Failed to check if note exists"
    end
    
    local num_rows = tonumber(result[1].num)

    -- Construct INSERT or UPDATE statement
    local stmt
    if num_rows > 0 then
        stmt = string.format([[
            UPDATE notes
            SET content = '%s'
            WHERE subject = '%s' AND name = '%s';
        ]], content, subject, title)
    else
        stmt = string.format([[
            INSERT INTO notes (subject, name, content)
            VALUES ('%s', '%s', '%s');
        ]], subject, title, content)
    end

    -- Execute the statement
    local success = state.database.execute(state.brain_file, stmt)
    if not success then
        return false, "Failed to update note from file: " .. note_path
    end

    -- Clear existing connections for this note
    local clear_links = string.format("DELETE FROM connections WHERE source = '%s';", title)
    success = state.database.execute(state.brain_file, clear_links)
    if not success then
        return false, "Failed to clear note links from file: " .. note_path
    end

    -- Insert updated links
    if #links > 0 then
        local insert_links = "INSERT INTO connections (source, target) VALUES "
        for i, link in ipairs(links) do
            insert_links = insert_links .. string.format("('%s', '%s')%s", title, link, i < #links and "," or ";")
        end
        success = state.database.execute(state.brain_file, insert_links)
        if not success then
            return false, "Failed to update note links from file: " .. note_path
        end
    end

    return true
end
function update.do_update(state)
    local arg_string = [[
        -f --file arg string false
    ]]

    local help_string = get_help_string(arg[0])
    local args = command_utils.parse_command_args(arg_string, help_string)

    if not args then
        return false
    end

    local success, err

    if args["file"] then
        success, err = update.update_note_from_file(state, args["file"])
    else
        success, err = update_from_vault(state)
    end

    if not success then
        command_utils.handle_command_error(err or "Unknown error", help_string)
        return false
    end

    return true
end

-- Export the module
return update
