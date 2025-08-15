-- Define a module table
local task = {}

local os = require("os")
local command_utils = require("command_utils")
local get_help_string = require("help").get_help_string
local bx_utils = require("bx_utils")

function check_overdue(due_to)
    local current_time = os.time()
    local year, month, day, hour, min, sec = due_to:match("(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)")
    local task_time = os.time{year=year, month=month, day=day, hour=hour, min=min, sec=sec}
    return current_time > task_time
end

function update_overdue(state)
    -- Query to get all unfinished tasks
    local query = "SELECT id, due_to FROM tasks WHERE done IS NULL;"
    local unfinished = state.database.query(state.brain_file, query)
    
    if not unfinished then
        return false, "Failed to query unfinished tasks"
    end

    for _, task in pairs(unfinished) do
        local overdue = check_overdue(task.due_to)
        if overdue then
            local update_statement = "UPDATE tasks SET overdue = 1 WHERE id = " .. task.id .. ";"
            local success = state.database.execute(state.brain_file, update_statement)
            if not success then
                return false, "Failed to update overdue status"
            end
        end
    end
    
    return true
end

function backup_tasks(state)
    if state.vault_path then
        local backup_path = joinpath(state.vault_path, "tasks.tsv")
        export_delimited(state.brain_file, "SELECT * FROM tasks;", backup_path, "\t", true)
    end
end

function add_task(state, args)
    -- get task info
    local subject = args["subject"] or "NULL"
    local content = args["content"] or ""
    local time_input_str = args["due_to"] or ""
    local due_to = normalize_datetime(time_input_str)

    if content == "" then
        return false, "Must provide task content"
    end

    if not due_to then
        local current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tomorrow
    elseif not is_valid_timestamp(due_to) then
        return false, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local id = bx_utils.generate_id(state, "tasks")
    local insert_statement = string.format([[
    INSERT INTO tasks (id, subject, content, due_to, overdue, done)
    VALUES ('%s', '%s', '%s', '%s', '%s', NULL);
    ]], id, subject, content, due_to, overdue)
    
    local success = state.database.execute(state.brain_file, insert_statement)
    if not success then
        return false, "Failed to insert task"
    end
    
    backup_tasks(state)
    return true
end
function list_tasks(state, args)
    local tasks_empty = bx_utils.is_sqlite_empty(state, "tasks")
    if tasks_empty then
        return false, "Empty task list"
    end

    local success, err = update_overdue(state)
    if not success then
        return false, err
    end

    local subject = args["subject"] or ""
    local time_input_str = args["due_to"] or ""
    local due_to = normalize_datetime(time_input_str)

    local query = "SELECT id, subject, content, due_to, overdue FROM tasks WHERE done IS NULL "
    if subject ~= "" then
        query = query .. string.format("AND subject = '%s'", subject)
    end

    if due_to then
        query = query .. string.format("AND due_to > '%s'", due_to)
    end

    query = query .. " ORDER BY due_to, subject;"

    local result = state.database.query(state.brain_file, query)
    if not result then
        return false, "Failed to query tasks"
    end
    
    if length(result) > 0 then
        view(result, {columns={"id", "subject", "content", "due_to", "overdue"}})
    else
        return false, "Empty task list"
    end
    
    return true
end

function mark_done(state, args)
    local task_id = args["id"] or ""
    local comment = args["comment"] or ""

    if task_id == "" then
        return false, "Must provide task id"
    end

    local update_statement = "UPDATE tasks SET done = CURRENT_TIMESTAMP, comment = '" .. comment .. "' WHERE id = " .. task_id .. ";"
    local success = state.database.execute(state.brain_file, update_statement)
    if not success then
        return false, "Failed to mark task as done"
    end
    
    backup_tasks(state)
    return true
end

function delay_due(state, args)
    local task_id = args["id"] or ""
    local time_input_str = args["due_to"] or ""
    local due_to = normalize_datetime(time_input_str)

    if not due_to then
        local current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tomorrow
    elseif not is_valid_timestamp(due_to) then
        return false, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local update_statement
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, overdue)
    else
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE id='%s';", due_to, overdue, task_id)
    end
    
    local success = state.database.execute(state.brain_file, update_statement)
    if not success then
        return false, "Failed to delay task"
    end
    
    backup_tasks(state)
    return true
end
function last_done(state, args)
    local subject = args["subject"] or ""
    local num = args["number"] or 5

    local query = "SELECT content, subject, comment FROM tasks WHERE done IS NOT NULL "
    if subject ~= "" then
        query = query .. string.format("AND subject='%s'", subject)
    end

    query = query .. " ORDER BY done DESC "

    if num ~= "" then
        query = query .. string.format("LIMIT %s", num)
    end

    local result = state.database.query(state.brain_file, query)
    if not result then
        return false, "Failed to query completed tasks"
    end
    
    if length(result) > 0 then
        view(result, {columns={"subject", "content", "comment"}})
    else
        return false, "No tasks to view"
    end
    
    return true
end

function task.do_task(state)
    local arg_string = [[
        -d --do arg string false
        -s --subject arg string false
        -t --due_to arg string false
        -i --id arg string false
        -m --comment arg string false
        -c --content arg string false
        -n --number arg number false
    ]]

    local help_string = get_help_string(arg[0])
    local args = command_utils.parse_command_args(arg_string, help_string)

    if not args then
        return false
    end

    local success, err

    if args["do"] == "add" then
        success, err = add_task(state, args)
    elseif args["do"] == "list" then
        success, err = list_tasks(state, args)
    elseif args["do"] == "done" then
        success, err = mark_done(state, args)
    elseif args["do"] == "delay" then
        success, err = delay_due(state, args)
    elseif args["do"] == "last" then
        success, err = last_done(state, args)
    elseif not args["do"] then
        success, err = add_task(state, args)
    else
        success, err = false, "Unknown subcommand: " .. args["do"]
    end

    if not success then
        command_utils.handle_command_error(err or "Unknown error", help_string)
        return false
    end

    return true
end

-- Export the module
return task
