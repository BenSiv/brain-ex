-- Define a module table
local note = {}

local sqlite = require("sqlite3")
local lfs = require("lfs")
local get_vault_path = require("bx_utils").get_vault_path
local user = require("user")

local function insert_note(brain_file, group, title, content)
    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. table.concat(content, "\n") .. "');"
    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
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
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
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

    local to_write = table.concat(content, "\n") .. "\n" .. table.concat(obsidian_links, "\n")
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

    local insert_status = insert_note(brain_file, group, title, content)
    if not insert_status then
        print("Error: note insertion failed")
        return
    end
    local connect_status = connect_notes(brain_file, title, links)
    if not connect_status then
        print("Error: notes connection failed")
        return
    end
    local write_status = write_note(vault_dir, group, title, content, links)
    if not write_status then
        print("Error: note writing to file failed")
        return
    end
    return "success"
end

note.take_note = take_note

-- Export the module
return note