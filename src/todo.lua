-- Define a module table
local todo = {}

local sqlite = require("sqlite3")
local os = require("os")

function check_overdue(due_to)
    local current_time = os.time()
    local year, month, day, hour, min, sec = due_to:match("(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)")
    local task_time = os.time{year=year, month=month, day=day, hour=hour, min=min, sec=sec}
    return current_time > task_time
end

function add_task(brain_file)
    -- get note info
    io.write("Task: ")
    local task = io.read()
    io.write("Due To: ")
    local time_input_str = io.read()
    local due_to = normalize_datetime(time_input_str)

    if not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local id = generate_id("todos")
    local insert_statement = "INSERT INTO todos (id, task, due_to, done, overdue) VALUES (" .. id .. ", '" .. task .. "', '" .. due_to .. "', '0', " .. overdue .. ");"

    -- write note info
    local db = sqlite.open(brain_file)
    db:exec(insert_statement)
    db:close()
end

function list_tasks(brain_file)
    local query = "SELECT id, task, due_to FROM todos WHERE done=0;"

    local todos_empty = is_sqlite_empty(brain_file, "todos")
    if todos_empty then
        print("Empty task list")
        return
    end

    -- write note info
    local db = sqlite.open(brain_file)
    local result_rows = {}
    for row in db:rows(query) do
        table.insert(result_rows, row)
    end

    if length(result_rows) > 0 then
        view(result_rows)
    else
        print("Empty task list")
    end

    db:close()
end

function mark_done(brain_file)
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