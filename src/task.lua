-- Define a module table
task = {}

os = require("os")
utils = require("utils")
argparse = require("argparse")
database = require("database")
local_update = database.local_update
local_query = database.local_query
config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
help = require("help")
dates = require("dates")
bx_utils = require("bx_utils")
generate_id = bx_utils.generate_id
dataframes = require("dataframes")
view = dataframes.view

function check_overdue(due_to)
    if due_to == nil or due_to == "" then
        return false
    end
    normalized = dates.normalize_datetime(due_to)
    if normalized == nil then
        return false
    end
    if dates.is_valid_timestamp != nil and dates.is_valid_timestamp(normalized) == false then
        return false
    end
    -- Use fixed-width slicing to avoid pattern issues in luam
    year = tonumber(string.sub(normalized, 1, 4))
    month = tonumber(string.sub(normalized, 6, 7))
    day = tonumber(string.sub(normalized, 9, 10))
    hour = tonumber(string.sub(normalized, 12, 13))
    min = tonumber(string.sub(normalized, 15, 16))
    sec = tonumber(string.sub(normalized, 18, 19))
    if year == nil or month == nil or day == nil or hour == nil or min == nil or sec == nil then
        return false
    end
    current_time = os.time()
    task_time = os.time({year=year, month=month, day=day, hour=hour, min=min, sec=sec})
    if task_time == nil then
        return false
    end
    return current_time > task_time
end

function update_overdue(brain_file)
    -- Query to get all unfinished tasks
    query = "SELECT id, due_to FROM tasks WHERE done IS NULL AND due_to IS NOT NULL;"
    unfinished = local_query(brain_file, query)

    overdue = false
    update_statement = ""
    if unfinished != nil then
        for _, task in pairs(unfinished) do
            task_id = task.id or task[1]
            task_due = task.due_to or task[2]
            overdue = check_overdue(task_due)
            if overdue then
                if task_id != nil then
                    update_statement = "UPDATE tasks SET overdue = 1 WHERE id = " .. task_id .. ";"
                else
                    update_statement = nil
                end
                success = local_update(brain_file, update_statement)
                if success == nil then
                    return nil, "Failed to update overdue status for task ID: " .. tostring(task_id)
                end
            end
        end
    end
    return true
end

function backup_tasks(brain_file)
    vault_path = get_vault_path()
    if vault_path != nil then
        backup_path = joinpath(vault_path, "tasks.tsv")
        export_delimited(brain_file, "SELECT * FROM tasks;", backup_path, "\t", true)
    end
    return true
end

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function add_task(brain_file, args)
    -- get note info
    subject = args["subject"]
    content = args["content"] or ""
    time_input_str = args["due_to"] or ""
    due_to = dates.normalize_datetime(time_input_str)

    if content == "" then
        return nil, "Must provide task content"
    end

	if due_to == nil then
		current_time = os.time()
		due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif dates.is_valid_timestamp(due_to) == false then
        return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
    end

    overdue_bool = check_overdue(due_to)
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end
    id = generate_id("tasks")
    
    esc_subject = "NULL"
    if subject != nil then
        esc_subject = "'" .. escape_sql(subject) .. "'"
    end
    esc_content = escape_sql(content)
    
    insert_statement = string.format("""
    INSERT INTO tasks (id, subject, content, due_to, overdue, done)
    VALUES ('%s', %s, '%s', '%s', '%s', NULL);
    """, id, esc_subject, esc_content, due_to, esc_overdue)
    -- write note info
    success = local_update(brain_file, insert_statement)
	if success == nil then
		return nil, "Failed to add task"
	end

    backup_tasks(brain_file)
    return true
end

function list_tasks(brain_file, args)
    tasks_empty = is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("No pending tasks")
        return true
    end
    
    update_overdue(brain_file)

    subject = args["subject"] or ""
    time_input_str = args["due_to"] or ""
    due_to = dates.normalize_datetime(time_input_str)

    query = "SELECT id, subject, content, due_to, overdue FROM tasks WHERE done IS NULL "
    if subject != "" then
        query = query .. string.format("AND subject = '%s'", escape_sql(subject))
    end

    if due_to != nil then
        query = query .. string.format("AND due_to > '%s'", due_to)
    end
    
    query = query .. " ORDER BY due_to, subject;"

    result = local_query(brain_file, query)
    if result != nil and length(result) > 0 then
        view(result, {columns={"id", "subject", "content", "due_to", "overdue"}})
    else
        print("No pending tasks")
    end
    return true
end

function mark_done(brain_file, args)
    task_id = args["id"] or ""
    comment = args["comment"] or ""

    if task_id == "" then
        return nil, "Must provide task id"
    end

    update_statement = "UPDATE tasks SET done = CURRENT_TIMESTAMP, comment = '" .. escape_sql(comment) .. "' WHERE id = " .. task_id .. ";"
    status = local_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to mark task as done"
    end
    backup_tasks(brain_file)
    return true
end

function delay_due(brain_file, args)
    task_id = args["id"] or ""
    time_input_str = args["due_to"] or ""
    due_to = dates.normalize_datetime(time_input_str)

    if due_to == nil then
        -- current_due_to = local_query(brain_file, "SELECT due_to FROM tasks WHERE id = '" .. task_id .. "'")
   		-- due_to = os.date("%Y-%m-%d %H:%M:%S", current_due_to + 86400) -- one day later
   		current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif dates.is_valid_timestamp(due_to) == false then
        return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
    end

    overdue_bool = check_overdue(due_to)
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end
    update_statement = nil
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, esc_overdue)
    else
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE id='%s';", due_to, esc_overdue, task_id)
    end
    status = local_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to delay task due date"
    end
    backup_tasks(brain_file)
    return true
end

function last_done(brain_file, args)
    subject = args["subject"] or ""
    num = args["number"] or 5

    query = "SELECT content, subject, comment FROM tasks WHERE done IS NOT NULL "
    if subject != "" then
        query = query .. string.format("AND subject='%s'", escape_sql(subject))
    end
    
    query = query .. " ORDER BY done DESC "

    if num != "" then
        query = query .. string.format("LIMIT %s", num)
    end

    result = local_query(brain_file, query)
    if length(result) > 0 then
        view(result, {columns={"subject", "content", "comment"}})
    else
        print("No tasks to view")
    end
    return true
end

function do_task(brain_file, cmd_args)
    -- print("Debug: cmd_args[1] IN: " .. tostring(cmd_args[1]))
    if cmd_args[1] != nil and string.sub(cmd_args[1], 1, 1) != "-" then
        table.insert(cmd_args, 1, "-d")
    end
    -- print("Debug: cmd_args[1] OUT: " .. tostring(cmd_args[1]))
    arg_string = """
        -d --do arg string false
        -s --subject arg string false
        -t --due_to arg string false
        -i --id arg string false
        -m --comment arg string false
        -c --content arg string false
        -n --number arg number false
    """

    help_string = help.get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)
    status, err = nil, nil
    if args != nil then
        if args["do"] == "add" then
            status, err = add_task(brain_file, args)
        elseif args["do"] == "list" then
            status, err = list_tasks(brain_file, args)
        elseif args["do"] == "done" then
            status, err = mark_done(brain_file, args)
        elseif args["do"] == "delay" then
            status, err = delay_due(brain_file, args)
        elseif args["do"] == "last" then
            status, err = last_done(brain_file, args)
        elseif args["do"] == nil then
            status, err = add_task(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, list, done, delay, last")
            return "success" -- Help printed
        end
    end
    if status != true then
        print(err or "Task command failed")
        return "error"
    end
    return "success"
end

task.do_task = do_task

if string.match(arg[0], "task.lua$") != nil then
    do_task(get_brain_path(), arg)
else
    -- Export the module
    return task
end
