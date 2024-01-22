-- Define a module table
local todo = {}

local sqlite = require("sqlite3")

local function get_brain_file()
	local home_dir = os.getenv("HOME")
    local dot_file = io.open(home_dir .. "/.bx", "r")
    local brain_file = dot_file:read("*a")
    return brain_file
end

function add_task()
    -- get database name
    local brain_file = get_brain_file()

    -- get note info
    io.write("Task: ")
    local task = "'" .. io.read() .. "'"
    io.write("Due To: ")
    local due_to = ""
    local time_input_str = io.read()
    if is_timestamp(time_input_str) then
        due_to = "'" .. time_input_str .. "'"
    else
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS")
        return
    end

    local id = generate_id("todos")

    local insert_statement = "INSERT INTO todos (id, task, due_to, done) VALUES (" .. id .. ", " .. task .. ", " .. due_to .. ", '0');"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

function list_tasks()
    -- get database name
    local brain_file = get_brain_file()

    local query = "SELECT id, task, due_to FROM todos WHERE done=0;"

    -- write note info
    local db = sqlite.open(brain_file)
    local result_rows = {}
    for row in db:rows(query) do
        table.insert(result_rows, row)
    end

    view(result_rows)

    db:close()
end

function mark_done()
    -- get database name
    local brain_file = get_brain_file()

    -- get note info
    io.write("Enter the ID of the task to mark as done: ")
    local task_id = io.read()

    local update_statement = "UPDATE todos SET done = 1 WHERE id = " .. task_id .. ";"

    -- update note info
    local db = sqlite.open(brain_file)
    db:exec(update_statement)
    db:close()
end

todo.add_task = add_task
todo.list_tasks = list_tasks
todo.mark_done = mark_done

-- Export the module
return todo