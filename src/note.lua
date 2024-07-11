-- Define a module table
local note = {}

local lfs = require("lfs")
local get_vault_path = require("bx_utils").get_vault_path
local user = require("user")

local function insert_note(brain_file, group, title, content)
    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. table.concat(content, "\n") .. "');"
    -- write note info
    local_update(brain_file, insert_statement)
    return "success"
end

local function append_content(brain_file, group, title, content)
    local query = string.format("SELECT content FROM notes WHERE [name]='%s' AND [group]='%s';", title, group)
    local result = local_query(brain_file, query)
    local new_content = result[1].content .. "\n" .. table.concat(content, "\n")

    local update_statement = string.format("UPDATE notes SET content='%s' WHERE [name]='%s' AND [group]='%s';", new_content, title, group)
    -- write note info
    local_update(brain_file, update_statement)
    return "success"
end

local function connect_notes(brain_file, source, links)
    local insert_statement = "INSERT INTO connections (source, target) VALUES "
    for index, target in pairs(links) do
        local statement_value = "('" .. source .. "', '" .. target .. "'), "
        insert_statement = insert_statement .. statement_value
    end
    insert_statement = slice(insert_statement, 1, length(insert_statement)-2) .. ";"
    -- write note info
    local_update(brain_file, insert_statement)
    return "success"
end

local function write_note(vault_dir, group, title, content, links)
    local obsidian_links = {}
    for index, link in pairs(links) do
        table.insert(obsidian_links, "[[" .. link .. "]] ")
    end

    local note_dir = vault_dir .. "/" .. group
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

    local to_write = table.concat(content, "\n") .. "\n" .. table.concat(obsidian_links, "\n") .. "\n"
    note_file:write(to_write)
    note_file:close()
    return "success"
end

function take_note(brain_file)
    -- get note info
    local group = user.input("Context group: ")
    local title = user.input("Title: ")
    local content = user.inputs("Content: ")
    local links = user.inputs("Links: ")

    local vault_dir = get_vault_path()

    if not isempty(content) then
        local insert_status = insert_note(brain_file, group, title, content)
        if not insert_status then
            print("Error: note insertion failed")
            return
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
        local write_status = write_note(vault_dir, group, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

function update_note(brain_file)
    -- get note info
    local group = user.input("Context group: ")
    local title = user.input("Title: ")
    local content = user.inputs("Content: ")
    local links = user.inputs("Links: ")

    local vault_dir = get_vault_path()

    if not isempty(content) then
        local append_status = append_content(brain_file, group, title, content)
        if not append_status then
            print("Error: append content failed")
            return
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
        local write_status = write_note(vault_dir, group, title, content, links)
        if not write_status then
            print("Error: note writing to file failed")
            return
        end
    end

    return "success"
end

note.take_note = take_note
note.update_note = update_note

-- Export the module
return note