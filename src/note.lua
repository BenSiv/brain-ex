-- Define a module table
local note = {}

local sqlite = require("sqlite3")
local path = require("path")
local get_vault_path = require("bx_utils").get_vault_path

local function get_note_id(brain_file, note)
    local db = sqlite.open(brain_file)

    local result_rows = {}
    local query = "SELECT time FROM notes WHERE name='" .. note .. "'"
    for row in db:rows(query) do
        table.insert(result_rows, row)
    end

    return result_rows[1]
end

local function connect_notes(brain_file, title, links)
    local insert_statement = "INSERT INTO connections (source, target) VALUES "
    local source = get_note_id(brain_file, title)
    for index, link in pairs(split(links, ", ")) do
        local target = get_note_id(brain_file, link)
        local statement_value = "('" .. source .. "', '" .. target .. "'), "
        insert_statement = insert_statement .. statement_value
    end
    insert_statement = slice(insert_statement, 1, length(insert_statement)-2) .. ";"
    
    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

local function write_note(vault_dir, group, title, content, links)
    local to_write = "content"
    local obsidian_links = ""
    for index, link in pairs(split(links, ", ")) do
        obsidian_links = obsidian_links .. "[[" .. link .. "]] "
    end
    note_path = path(vault_dir, group, title .. ".md")
    local note_file = io.open(note_path, "w")
    note_file:write(content .. "\n")
    note_file:write(obsidian_links)
    note_file:close()
end

function take_note(brain_file)
    -- get note info
    io.write("Context group: ")
    local group = "'" .. io.read() .. "'"
    io.write("Title: ")
    local title = "'" .. io.read() .. "'"
    io.write("Content: ")
    local content = "'" .. io.read() .. "'"
    io.write("Links: ")
    local links = io.read()

    local vault_dir = get_vault_path()

    connect_notes(brain_file, title, links)
    write_note(vault_dir, group, title, content, links)

    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. content .. "');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

note.take_note = take_note

-- Export the module
return note