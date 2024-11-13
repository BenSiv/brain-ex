-- Define a module table
local task = {}

local sqlite = require("sqlite3")
local os = require("os")
local get_vault_path = require("bx_utils").get_vault_path

function check_overdue(due_to)
    local current_time = os.time()
    local year, month, day, hour, min, sec = due_to:match("(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)")
    local task_time = os.time{year=year, month=month, day=day, hour=hour, min=min, sec=sec}
    return current_time > task_time
end

function update_overdue(brain_file)
    -- Query to get all unfinished tasks
    local query = "SELECT id, due_to FROM tasks WHERE done=0;"
    local unfinished = local_query(brain_file, query)

    local overdue = false
    local update_statement = ""
    for _, task in pairs(unfinished) do
        overdue = check_overdue(task.due_to)
        if overdue then
            update_statement = "UPDATE tasks SET overdue = 1 WHERE id = " .. task.id .. ";"
            local_update(brain_file, update_statement)
        end
    end
end

function backup_tasks(brain_file)
    local vault_dir = get_vault_path()
    if vault_dir then
        local backup_path = joinpath(vault_dir, "tasks.tsv")
        export_delimited(brain_file, "SELECT * FROM tasks;", backup_path, "\t", true)
    end
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
    local id = generate_id("tasks")
    local insert_statement = "INSERT INTO tasks (id, task, due_to, overdue, done) VALUES (" .. id .. ", '" .. task .. "', '" .. due_to .. "', '" .. overdue .. "', '0');"
    -- write note info
    local_update(brain_file, insert_statement)
    backup_tasks(brain_file)
end

function list_tasks(brain_file)
    update_overdue(brain_file)

    local query = "SELECT id, task, due_to, overdue FROM tasks WHERE done=0 order by due_to;"

    local tasks_empty = is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("Empty task list")
        return
    end

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
    local update_statement = "UPDATE tasks SET done = 1 WHERE id = " .. task_id .. ";"
    local_update(brain_file, update_statement)
    backup_tasks(brain_file)
end

-- Add the function to the module
task.add_task = add_task
task.list_tasks = list_tasks
task.mark_done = mark_done

-- Export the module
return task