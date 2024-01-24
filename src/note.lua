-- Define a module table
local note = {}

local sqlite = require("sqlite3")

function take_note(brain_file)
    -- get note info
    io.write("Context group: ")
    local group = "'" .. io.read() .. "'"
    io.write("Title: ")
    local title = "'" .. io.read() .. "'"
    io.write("Content: ")
    local content = "'" .. io.read() .. "'"

    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. content .. "');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

function connect_notes(brain_file)
    -- get note info
    io.write("Source note id: ")
    local source = "'" .. io.read() .. "'"
    io.write("Target note id: ")
    local target = "'" .. io.read() .. "'"

    local insert_statement = "INSERT INTO connections (source, target) VALUES ('" .. source .. "', '" .. target .. "');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

note.take_note = take_note

-- Export the module
return note