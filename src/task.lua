-- Define a module table
local task = {}

local sqlite = require("sqlite3")
local os = require("os")
local get_vault_path = require("bx_utils").get_vault_path
local user = require("user")

function check_overdue(due_to)
    local current_time = os.time()
    local year, month, day, hour, min, sec = due_to:match("(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)")
    local task_time = os.time{year=year, month=month, day=day, hour=hour, min=min, sec=sec}
    return current_time > task_time
end

function update_overdue(brain_file)
    -- Query to get all unfinished tasks
    local query = "SELECT id, due_to FROM tasks WHERE done IS NULL;"
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

function add_task(brain_file, args)
    -- get note info
    local subject = args["subject"] or "NULL"
    local content = args["content"] or ""
    local time_input_str = args["due_to"] or ""
    local due_to = normalize_datetime(time_input_str)

    if content == "" then
        print("Must provide task content")
        return
    end

	if not due_to then
		local current_time = os.time()
		due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local id = generate_id("tasks")
    local insert_statement = string.format([[
    INSERT INTO tasks (id, subject, content, due_to, overdue, done)
    VALUES ('%s', '%s', '%s', '%s', '%s', NULL);
    ]], id, subject, content, due_to, overdue)
    -- write note info
    local_update(brain_file, insert_statement)
    backup_tasks(brain_file)
end

function list_tasks(brain_file, args)
    update_overdue(brain_file)

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
    
    local tasks_empty = is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("Empty task list")
        return
    end

    result = local_query(brain_file, query)
    if length(result) > 0 then
        view(result, {columns={"id", "subject", "content", "due_to", "overdue"}})
    else
        print("Empty task list")
    end
end

function mark_done(brain_file, args)
    local task_id = args["id"] or ""
    local comment = args["comment"] or ""

    if content == "" then
        print("Must provide task id")
        return
    end

    local update_statement = "UPDATE tasks SET done = CURRENT_TIMESTAMP, comment = '" .. comment .. "' WHERE id = " .. task_id .. ";"
    local_update(brain_file, update_statement)
    backup_tasks(brain_file)
end

function delay_due(brain_file, args)
    local task_id = args["id"] or ""
    local time_input_str = args["due_to"] or ""
    local due_to = normalize_datetime(time_input_str)

    if not due_to then
   		local current_time = os.time()
   		due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    local overdue = check_overdue(due_to) and 1 or 0
    local update_statement
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, overdue)
    else
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE id='%s';", due_to, overdue, task_id)
    end
    local_update(brain_file, update_statement)
    backup_tasks(brain_file)
end

function last_done(brain_file, args)
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

    result = local_query(brain_file, query)
    if length(result) > 0 then
        view(result, {columns={"subject", "content", "comment"}})
    else
        print("No tasks to view")
    end
end

local function get_help_string(subcommand)
    local help_strings = {
        ["brex task"] = [[
            Description:
            Adds a new task. The due date can be specified in the format yyyy-mm-dd HH:MM:SS, or part of it.

            Required:
            -c --content <content> Task's content.
            
            Optional:
            -s --subject <subject> Task's subject, defaults to NULL.
            -t --due_to <due_to> Task's due date in the format yyyy-mm-dd, defaults to 24 hours from now.
            
            Examples:
            brex task add --content "This is a new task"
            brex task --content "This is a work task" --subject "work" --due_to "2024-12-31"
        ]],
        ["brex task add"] = [[
            Description:
            Adds a new task. The due date can be specified in the format yyyy-mm-dd HH:MM:SS, or part of it.

            Required:
            -c --content <content> Task's content.
            
            Optional:
            -s --subject <subject> Task's subject, defaults to NULL.
            -t --due_to <due_to> Task's due date in the format yyyy-mm-dd, defaults to 24 hours from now.
            
            Examples:
            brex task add --content "This is a new task"
            brex task --content "This is a work task" --subject "work" --due_to "2024-12-31"
        ]],
        ["brex task list"] = [[
            Description:
            Lists all tasks that are not done yet.

            Optional: 
            -s --subject <subject> Filter tasks by subject.
            -t --due_to <due_to> Filter tasks by due date in the format yyyy-mm-dd.

            Example:
            brex task list
            brex task list --subject "work"
            brex task list --due_to "2024-12-31"
        ]],
        ["brex task done"] = [[
            Description:
            Marks a task as done by its ID and optionally adds a comment.

            Required:
            -i --id <id> ID of the task to mark as done.

            Optional:
            -m --comment <comment> Comment to add when marking the task as done.

            Example:
            brex task done
            brex task done --comment "This task is completed"            
        ]],
        ["brex task delay"] = [[
            Description:
            Delays a task's due time, pass * for all tasks.

            Required:
            -i --id <id> ID of the task to delay, or * to delay all tasks.

            Optional:
            -t --due_to <due_to> New due date in the format yyyy-mm-dd HH:MM:SS, or part of it. If not provided, defaults to 24 hours from now.

            Example:
            brex task delay --id 85560914 --due_to "2024-12-31"
            brex task delay --id *
        ]]
    }

    return help_strings[subcommand]
end


local function do_task(brain_file)
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

    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    if args then
        if args["do"] == "add" then
            add_task(brain_file, args)
        elseif args["do"] == "list" then
            list_tasks(brain_file, args)
        elseif args["do"] == "done" then
            mark_done(brain_file, args)
        elseif args["do"] == "delay" then
            delay_due(brain_file, args)
        elseif args["do"] == "last" then
            last_done(brain_file, args)
        elseif not args["do"] then
            add_task(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, list, done, delay, last")
        end
    end
end

task.do_task = do_task

if arg[0] == "task.lua" then
    do_task(todays_task)
else
    -- Export the module
    return task
end
