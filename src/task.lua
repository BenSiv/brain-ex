-- Define a module table
task = {}

os = require("os")

function check_overdue(due_to)
    if not due_to or due_to == "" then
        return false
    end
    current_time = os.time()
    year, month, day, hour, min, sec = string.match(due_to, "(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)")
    if not year then
        return false
    end
    task_time = os.time({year=year, month=month, day=day, hour=hour, min=min, sec=sec})
    return current_time > task_time
end

function update_overdue(brain_file)
    -- Query to get all unfinished tasks
    query = "SELECT id, due_to FROM tasks WHERE done IS NULL AND due_to IS NOT NULL;"
    unfinished = local_query(brain_file, query)

    overdue = false
    update_statement = ""
    if unfinished then
        for _, task in pairs(unfinished) do
            overdue = check_overdue(task.due_to)
            if overdue then
                update_statement = "UPDATE tasks SET overdue = 1 WHERE id = " .. task.id .. ";"
                success = local_update(brain_file, update_statement)
                if not success then
                    print("Failed to update overdue status for task ID: " .. task.id)
                    return
                end
            end
        end
    end
    return "success"
end

function backup_tasks(brain_file)
    vault_path = get_vault_path()
    if vault_path then
        backup_path = joinpath(vault_path, "tasks.tsv")
        export_delimited(brain_file, "SELECT * FROM tasks;", backup_path, "\t", true)
    end
    return "success"
end

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function add_task(brain_file, args)
    -- get note info
    subject = args["subject"]
    content = args["content"] or ""
    time_input_str = args["due_to"] or ""
    due_to = normalize_datetime(time_input_str)

    if content == "" then
        print("Must provide task content")
        return
    end

	if not due_to then
		current_time = os.time()
		due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    overdue = check_overdue(due_to) and 1 or 0
    id = generate_id("tasks")
    
    esc_subject = "NULL"
    if subject then
        esc_subject = "'" .. escape_sql(subject) .. "'"
    end
    esc_content = escape_sql(content)
    
    insert_statement = string.format("""
    INSERT INTO tasks (id, subject, content, due_to, overdue, done)
    VALUES ('%s', %s, '%s', '%s', '%s', NULL);
    """, id, esc_subject, esc_content, due_to, overdue)
    -- write note info
    success = local_update(brain_file, insert_statement)
	if not success then
		print("Failed to add task")
		return
	end

    backup_tasks(brain_file)
    return "success"
end

function list_tasks(brain_file, args)
    tasks_empty = is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("No pending tasks")
        return
    end
    
    update_overdue(brain_file)

    subject = args["subject"] or ""
    time_input_str = args["due_to"] or ""
    due_to = normalize_datetime(time_input_str)

    query = "SELECT id, subject, content, due_to, overdue FROM tasks WHERE done IS NULL "
    if subject != "" then
        query = query .. string.format("AND subject = '%s'", escape_sql(subject))
    end

    if due_to then
        query = query .. string.format("AND due_to > '%s'", due_to)
    end
    
    query = query .. " ORDER BY due_to, subject;"

    result = local_query(brain_file, query)
    if result and length(result) > 0 then
        view(result, {columns={"id", "subject", "content", "due_to", "overdue"}})
    else
        print("No pending tasks")
    end
    return "success"
end

function mark_done(brain_file, args)
    task_id = args["id"] or ""
    comment = args["comment"] or ""

    if task_id == "" then
        print("Must provide task id")
        return
    end

    update_statement = "UPDATE tasks SET done = CURRENT_TIMESTAMP, comment = '" .. escape_sql(comment) .. "' WHERE id = " .. task_id .. ";"
    status = local_update(brain_file, update_statement)
    if not status then
        print("Failed to mark task as done")
        return
    end
    backup_tasks(brain_file)
    return "success"
end

function delay_due(brain_file, args)
    task_id = args["id"] or ""
    time_input_str = args["due_to"] or ""
    due_to = normalize_datetime(time_input_str)

    if not due_to then
        -- current_due_to = local_query(brain_file, "SELECT due_to FROM tasks WHERE id = '" .. task_id .. "'")
   		-- due_to = os.date("%Y-%m-%d %H:%M:%S", current_due_to + 86400) -- one day later
   		current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    elseif not is_valid_timestamp(due_to) then
        print("Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it")
        return
    end

    overdue = check_overdue(due_to) and 1 or 0
    update_statement = nil
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, overdue)
    else
        update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE id='%s';", due_to, overdue, task_id)
    end
    status = local_update(brain_file, update_statement)
    if not status then
        print("Failed to delay task due date")
        return
    end
    backup_tasks(brain_file)
    return "success"
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
    return "success"
end

function do_task(brain_file, cmd_args)
    -- print("Debug: cmd_args[1] IN: " .. tostring(cmd_args[1]))
    if cmd_args[1] and string.sub(cmd_args[1], 1, 1) != "-" then
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

    help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)
    status = nil
    if args then
        if args["do"] == "add" then
            status = add_task(brain_file, args)
        elseif args["do"] == "list" then
            status = list_tasks(brain_file, args)
        elseif args["do"] == "done" then
            status = mark_done(brain_file, args)
        elseif args["do"] == "delay" then
            status = delay_due(brain_file, args)
        elseif args["do"] == "last" then
            status = last_done(brain_file, args)
        elseif not args["do"] then
            status = add_task(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, list, done, delay, last")
        end
    end
    if status != "success" then
        print("Task command failed")
    end
    return "success"
end

task.do_task = do_task

if arg[0] == "task.lua" then
    do_task(brain_file)
else
    -- Export the module
    return task
end
