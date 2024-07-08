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
    local task = input("Task: ")
    local time_input_str = input("Due To: ")
    local due_to = normalize_datetime(time_input_str)

    if not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local id = generate_id("todos")
    local insert_statement = "INSERT INTO todos (id, task, due_to, overdue, done) VALUES (" .. id .. ", '" .. task .. "', '" .. due_to .. "', '" .. overdue .. "', '0');"
    -- write note info
    local_update(brain_file, insert_statement)
end

function list_tasks(brain_file)
    local query = "SELECT id, task, due_to, overdue FROM todos WHERE done=0;"

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
    db:close()
    
    if length(result_rows) > 0 then
        view(result_rows)
    else
        print("Empty task list")
    end
end

function mark_done(brain_file)
    local task_id = input("Enter the ID of the task to mark as done: ")
    local update_statement = "UPDATE todos SET done = 1 WHERE id = " .. task_id .. ";"
    local_update(brain_file, update_statement)
end

todo.add_task = add_task
todo.list_tasks = list_tasks
todo.mark_done = mark_done

-- Export the module
return todo