-- Define a module table
local note = {}

local sqlite = require("sqlite3")

local function get_brain_file()
	local home_dir = os.getenv("HOME")
    local dot_file = io.open(home_dir .. "/.bx", "r")
    local brain_file = dot_file:read("*a")
    return brain_file
end

function take_note()
    -- get database name
    local brain_file = get_brain_file()

    -- get note info
    io.write("Title: ")
    local title = "'" .. io.read() .. "'"
    io.write("Content: ")
    local content = "'" .. io.read() .. "'"

    local id = generate_id("notes")

    local insert_statement = "INSERT INTO notes (id, title, content) VALUES (" .. id .. ", " .. title .. ", " .. content .. ");"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

function connect_notes()
    -- get database name
    local brain_file = get_brain_file()

    -- get note info
    io.write("Source note id: ")
    local source = "'" .. io.read() .. "'"
    io.write("Target note id: ")
    local target = "'" .. io.read() .. "'"

    local id = generate_id("connections")

    local insert_statement = "INSERT INTO connections (id, source, target) VALUES (" .. id .. ", " .. source .. ", " .. target .. ");"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

note.take_note = take_note

-- Export the module
return note