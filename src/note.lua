-- Define a module table
local note = {}

local sqlite = require("sqlite3")

function get_note_id(brain_file, note)
    local db = sqlite.open(brain_file)

    local result_rows = {}
    local query = "SELECT time FROM notes WHERE name='" .. note .. "'"
    for row in db:rows(query) do
        table.insert(result_rows, row)
    end

    return result_rows[1]
end

function connect_notes(brain_file, note, links)
    local insert_statement = "INSERT INTO connections (source, target) VALUES "
    local source = get_note_id(brain_file, note)
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

    connect_notes(brain_file, title, links)
    write_note() -- write the markdwon content including timestamp

    local insert_statement = "INSERT INTO notes ('group', 'name', 'content') VALUES ('" .. group .. "', '" .. title .. "', '" .. content .. "');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

note.take_note = take_note

-- Export the module
return note