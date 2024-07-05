-- Define a module table
local note = {}

local sqlite = require("sqlite3")
local lfs = require("lfs")
local get_vault_path = require("bx_utils").get_vault_path

local function connect_notes(brain_file, source, links)
    local insert_statement = "INSERT INTO connections (source, target) VALUES "
    for index, target in pairs(split(links, ", ")) do
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
    local obsidian_links = ""
    for index, link in pairs(split(links, "\n")) do
        obsidian_links = obsidian_links .. "[[" .. link .. "]] "
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

    local to_write = content .. "\n" .. obsidian_links
    print(to_write)
    note_file:write(to_write)
    note_file:close()
    return "success"
end

function user_input(prompt)
    io.write(prompt)
    local answer = io.read()
    return answer
end

function user_inputs(prompt)
    io.write(prompt)
    local full_answer = ""
    local answer = ""
    while true do
        answer = io.read()
        if answer == "" then
            break
        end
        full_answer = full_answer .. "\n" .. answer
    end
    return full_answer
end

function take_note(brain_file)
    -- get note info
    local group = user_input("Context group: ")
    local title = user_input("Title: ")
    local content = user_inputs("Content: ")
    local links = user_inputs("Links: ")

    local vault_dir = get_vault_path()

    local connect_status = connect_notes(brain_file, title, links)
    if not connect_status then
        return
    end
    local write_status = write_note(vault_dir, group, title, content, links)
    if not write_status then
        return
    end

    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. content .. "');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

note.take_note = take_note

-- Export the module
return note