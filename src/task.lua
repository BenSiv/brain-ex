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
sync = require("sync")

function column_exists(brain_file, table_name, column_name)
    query = "PRAGMA table_info(" .. table_name .. ");"
    columns = local_query(brain_file, query)
    if columns == nil then
        return false
    end
    for _, col in pairs(columns) do
        if col["name"] == column_name or col[2] == column_name then
            return true
        end
    end
    return false
end

function ensure_priority_columns(brain_file)
    check_table = "SELECT name FROM sqlite_master WHERE type='table' AND name='tasks';"
    if local_query(brain_file, check_table) == nil or #local_query(brain_file, check_table) == 0 then
        return
    end

    if not column_exists(brain_file, "tasks", "owner") then
        local_update(brain_file, "ALTER TABLE tasks ADD COLUMN owner TEXT;")
    end
    if not column_exists(brain_file, "tasks", "importance") then
        local_update(brain_file, "ALTER TABLE tasks ADD COLUMN importance INTEGER DEFAULT 1;")
    end
    if not column_exists(brain_file, "tasks", "urgency") then
        local_update(brain_file, "ALTER TABLE tasks ADD COLUMN urgency INTEGER DEFAULT 1;")
    end
end

function check_overdue(due_to)
    if due_to == nil or due_to == "" then
        return false
    end
    normalized = dates.normalize_datetime(due_to)
    if normalized == nil then
        return false
    end
    if dates.is_valid_timestamp  !=  nil and dates.is_valid_timestamp(normalized) == false then
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
    if unfinished  !=  nil then
        for _, task in pairs(unfinished) do
            task_id = task.id or task[1]
            task_due = task.due_to or task[2]
            overdue = check_overdue(task_due)
            if overdue then
                if task_id  !=  nil then
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
        paths_mod = require("paths")
        tasks_dir = joinpath(vault_path, "tasks")
        paths_mod.create_dir_if_not_exists(tasks_dir)
        
        all_tasks = local_query(brain_file, "SELECT id, time, content, subject, due_to, overdue, done, comment, owner, importance, urgency FROM tasks;") or {}
        
        seen_files = {}
        for _, task_row in ipairs(all_tasks) do
            id = task_row.id or task_row[1]
            time_val = task_row.time or task_row[2]
            content = task_row.content or task_row[3] or ""
            subject = task_row.subject or task_row[4]
            due_to = task_row.due_to or task_row[5]
            overdue = task_row.overdue or task_row[6]
            done = task_row.done or task_row[7]
            comment = task_row.comment or task_row[8]
            owner = task_row.owner or task_row[9]
            importance = task_row.importance or task_row[10]
            urgency = task_row.urgency or task_row[11]
            
            metadata = {
                id = id,
                time = time_val,
                subject = subject,
                due_to = due_to,
                overdue = overdue,
                done = done,
                comment = comment,
                owner = owner,
                importance = importance,
                urgency = urgency
            }
            
            frontmatter = bx_utils.serialize_frontmatter(metadata)
            filename = id .. ".md"
            if subject != nil and subject != "NULL" and subject != "" then
                filename = subject .. "/" .. id .. ".md"
            end
            file_path = joinpath(tasks_dir, filename)
            seen_files[filename] = true
            
            dir_path = string.match(file_path, "(.*)/[^/]+$")
            if dir_path != nil then
                bx_utils.ensure_dir(dir_path)
            end
            
            f = io.open(file_path, "w")
            if f != nil then
                io.write(f, frontmatter)
                io.write(f, content)
                io.close(f)
                
                -- Update sync_meta with modification time of this file
                local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
                lfs_mod = require("lfs")
                attr = lfs_mod.attributes(file_path)
                if attr != nil then
                    local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_file_mod_%s', '%s');",
                        filename,
                        tostring(attr.modification)
                    ))
                    local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_file_size_%s', '%s');",
                        filename,
                        tostring(attr.size)
                    ))
                    local_update(brain_file, string.format(
                        "INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_id_for_%s', '%s');",
                        filename,
                        tostring(id)
                    ))
                end
            end
        end
        
        -- Clean up stale files in tasks_dir
        file_list = bx_utils.find_markdown_files(tasks_dir) or {}
        for _, item in ipairs(file_list) do
            file = item.rel_path
            if seen_files[file] == nil then
                os.remove(joinpath(tasks_dir, file))
            end
        end
        
        -- Clean up empty subdirectories
        lfs_mod = require("lfs")
        dirs = {}
        queue = {""}
        q_index = 1
        while q_index <= #queue do
            curr = queue[q_index]
            q_index = q_index + 1
            path = curr == "" and tasks_dir or tasks_dir .. "/" .. curr
            for f in lfs_mod.dir(path) do
                if f != "." and f != ".." then
                    rel = curr == "" and f or curr .. "/" .. f
                    full = tasks_dir .. "/" .. rel
                    attr = lfs_mod.attributes(full)
                    if attr != nil and attr.mode == "directory" then
                        table.insert(queue, rel)
                        table.insert(dirs, rel)
                    end
                end
            end
        end
        for i = #dirs, 1, -1 do
            lfs_mod.rmdir(tasks_dir .. "/" .. dirs[i])
        end
    end
    return true
end

function persist_tasks(brain_file)
    status, err = backup_tasks(brain_file)
    if status == nil then
        return nil, err
    end
    return sync.refresh(brain_file)
end

function escape_sql(str)
    return string.gsub(str, "'", "''")
end

function add_task(brain_file, args)
    ensure_priority_columns(brain_file)
    -- get note info
    subject = args["subject"]
    owner = args["owner"]
    content = args["content"] or ""
    
    importance = tonumber(args["importance"]) or 1
    urgency = tonumber(args["urgency"]) or 1
    
    if importance < 1 or importance > 5 then
        return nil, "Importance must be an integer between 1 and 5"
    end
    if urgency < 1 or urgency > 5 then
        return nil, "Urgency must be an integer between 1 and 5"
    end

    due_to = nil
    if args["due_to"] != nil then
        due_to = dates.normalize_datetime(args["due_to"])
        if due_to == nil then
            return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
        end
    end

    if content == "" then
        return nil, "Must provide task content"
    end

    overdue_bool = false
    if due_to != nil then
        overdue_bool = check_overdue(due_to)
    end
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end
    id = generate_id("tasks", nil, nil, brain_file)
    
    esc_subject = "NULL"
    if subject  !=  nil then
        esc_subject = "'" .. escape_sql(subject) .. "'"
    end
    esc_owner = "NULL"
    if owner  !=  nil and owner  !=  "" then
        esc_owner = "'" .. escape_sql(owner) .. "'"
    end
    esc_content = escape_sql(content)
    
    esc_due_to = "NULL"
    if due_to != nil then
        esc_due_to = "'" .. escape_sql(due_to) .. "'"
    end
    
    insert_statement = string.format("""
    INSERT INTO tasks (id, subject, content, due_to, overdue, done, owner, importance, urgency)
    VALUES ('%s', %s, '%s', %s, '%s', NULL, %s, %d, %d);
    """, id, esc_subject, esc_content, esc_due_to, esc_overdue, esc_owner, importance, urgency)
    -- write note info
    success = local_update(brain_file, insert_statement)
	if success == nil then
		return nil, "Failed to add task"
	end

    return persist_tasks(brain_file)
end

function list_tasks(brain_file, args)
    ensure_priority_columns(brain_file)
    tasks_empty = is_sqlite_empty(brain_file, "tasks")
    if tasks_empty then
        print("No pending tasks")
        return true
    end
    
    update_overdue(brain_file)

    subject = args["subject"] or ""
    owner = args["owner"] or ""
    
    due_to = nil
    if args["due_to"] != nil then
        due_to = dates.normalize_datetime(args["due_to"])
        if due_to == nil then
            return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it"
        end
    end

    query = """
    SELECT
        id,
        time,
        subject,
        content,
        due_to,
        overdue,
        done,
        comment,
        owner,
        urgency,
        COALESCE(importance, 1) AS importance,
        COALESCE(urgency, 1) AS manual_urgency,
        COALESCE(
            CASE
                WHEN due_to IS NULL THEN COALESCE(urgency, 1)
                ELSE CASE
                    WHEN (julianday(due_to) - julianday('now', 'localtime')) <= 1 THEN 5
                    WHEN (julianday(due_to) - julianday('now', 'localtime')) <= 2 THEN
                        CASE WHEN COALESCE(urgency, 1) > 4 THEN COALESCE(urgency, 1) ELSE 4 END
                    WHEN (julianday(due_to) - julianday('now', 'localtime')) <= 4 THEN
                        CASE WHEN COALESCE(urgency, 1) > 3 THEN COALESCE(urgency, 1) ELSE 3 END
                    WHEN (julianday(due_to) - julianday('now', 'localtime')) <= 7 THEN
                        CASE WHEN COALESCE(urgency, 1) > 2 THEN COALESCE(urgency, 1) ELSE 2 END
                    ELSE COALESCE(urgency, 1)
                END
            END,
            1
        ) AS active_urgency
    FROM tasks
    WHERE done IS NULL
    """

    if subject  !=  "" then
        query = query .. string.format(" AND subject = '%s'", escape_sql(subject))
    end

    if owner  !=  "" then
        query = query .. string.format(" AND owner = '%s' ", escape_sql(owner))
    end

    if due_to  !=  nil then
        query = query .. string.format(" AND due_to > '%s'", due_to)
    end
    
    query = query .. """
     ORDER BY
        (active_urgency * COALESCE(importance, 1)) DESC,
        COALESCE(importance, 1) DESC,
        active_urgency DESC,
        CASE WHEN due_to IS NULL THEN 1 ELSE 0 END ASC,
        due_to ASC,
        subject ASC;
    """

    result = local_query(brain_file, query)
    if result  !=  nil and length(result) > 0 then
        -- Load settings
        settings = config.load_settings()
        
        -- Resolve colors
        defaults = {
            Q1 = "\027[31m",
            Q2 = "\027[38;5;208m",
            Q3 = "\027[33m",
            Q4 = "\027[90m",
            reset = "\027[0m"
        }
        user_colors = settings.colors or {}
        colors = {
            Q1 = user_colors.Q1 or defaults.Q1,
            Q2 = user_colors.Q2 or defaults.Q2,
            Q3 = user_colors.Q3 or defaults.Q3,
            Q4 = user_colors.Q4 or defaults.Q4,
            reset = user_colors.reset or defaults.reset
        }

        -- Resolve columns
        cols = settings.task_columns
        
        -- Handle hide_due_to deprecation warning
        hide_due = false
        cfg = config.get_config()
        if cfg  !=  nil and cfg["hide_due_to"]  !=  nil then
            io.write(io.stderr, "Warning: 'hide_due_to' configuration in config.yaml is deprecated. Please define 'task_columns' in settings.json instead.\n")
            if cfg["hide_due_to"] == true or cfg["hide_due_to"] == "true" then
                hide_due = true
            end
        end

        if cols == nil then
            -- Fallback to default logic
            cols = {"id", "priority", "subject", "content"}
            if not hide_due then
                table.insert(cols, "due_to")
            end
        else
            if hide_due then
                filtered_cols = {}
                for _, col in ipairs(cols) do
                    if col != "due_to" then
                        table.insert(filtered_cols, col)
                    end
                end
                cols = filtered_cols
            end
        end

        for _, task_row in ipairs(result) do
            imp = tonumber(task_row.importance) or 1
            urg = tonumber(task_row.active_urgency) or 1
            
            quadrant = 4
            color_code = colors.Q4
            if imp >= 4 and urg >= 4 then
                quadrant = 1
                color_code = colors.Q1
            elseif imp >= 4 and urg < 4 then
                quadrant = 2
                color_code = colors.Q2
            elseif imp < 4 and urg >= 4 then
                quadrant = 3
                color_code = colors.Q3
            end
            
            reset_code = colors.reset
            priority_text = "Q" .. quadrant .. " (I:" .. imp .. " U:" .. urg .. ")"
            
            -- If due_to is overdue, we want to show [OVERDUE] tag
            due_str = tostring(task_row.due_to or "")
            if task_row.overdue == 1 or task_row.overdue == "1" then
                due_str = due_str .. " [OVERDUE]"
            end
            
            -- Apply the priority color to the selected columns for this row
            for _, col in ipairs(cols) do
                val = task_row[col]
                if col == "priority" then
                    val = priority_text
                elseif col == "due_to" then
                    val = due_str
                else
                    val = tostring(val or "")
                end
                task_row[col] = color_code .. val .. reset_code
            end
        end
        view(result, {columns=cols, line_length=999})
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
    return persist_tasks(brain_file)
end

function delay_due(brain_file, args)
    task_id = args["id"] or ""
    
    due_to = nil
    is_indefinite = false
    if args["due_to"] != nil then
        if args["due_to"] == "indefinitely" then
            is_indefinite = true
        else
            due_to = dates.normalize_datetime(args["due_to"])
            if due_to == nil then
                return nil, "Due To must conform to time-stamp format yyyy-mm-dd HH:MM:SS or a part of it, or 'indefinitely'"
            end
        end
    else
        current_time = os.time()
        due_to = os.date("%Y-%m-%d %H:%M:%S", current_time + 86400) -- tommorow
    end

    overdue_bool = false
    if is_indefinite == false then
        overdue_bool = check_overdue(due_to)
    end
    esc_overdue = 0
    if overdue_bool == true then
        esc_overdue = 1
    end
    update_statement = nil
    if task_id == "*" then
        if is_indefinite then
            update_statement = "UPDATE tasks SET due_to=NULL, overdue=0 WHERE done IS NULL;"
        else
            update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE done IS NULL;", due_to, esc_overdue)
        end
    else
        if is_indefinite then
            update_statement = string.format("UPDATE tasks SET due_to=NULL, overdue=0 WHERE id='%s';", task_id)
        else
            update_statement = string.format("UPDATE tasks SET due_to='%s', overdue='%s' WHERE id='%s';", due_to, esc_overdue, task_id)
        end
    end
    status = local_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to delay task due date"
    end
    return persist_tasks(brain_file)
end

function update_priority(brain_file, args)
    task_id = args["id"] or ""
    importance_str = args["importance"]
    urgency_str = args["urgency"]

    if task_id == "" then
        return nil, "Must provide task id"
    end

    if importance_str == nil and urgency_str == nil then
        return nil, "Must provide importance or urgency to update"
    end

    updates = {}
    if importance_str != nil then
        importance = tonumber(importance_str)
        if importance == nil or importance < 1 or importance > 5 then
            return nil, "Importance must be an integer between 1 and 5"
        end
        table.insert(updates, "importance=" .. importance)
    end

    if urgency_str != nil then
        urgency = tonumber(urgency_str)
        if urgency == nil or urgency < 1 or urgency > 5 then
            return nil, "Urgency must be an integer between 1 and 5"
        end
        table.insert(updates, "urgency=" .. urgency)
    end

    update_statement = nil
    if task_id == "*" then
        update_statement = string.format("UPDATE tasks SET %s WHERE done IS NULL;", table.concat(updates, ", "))
    else
        update_statement = string.format("UPDATE tasks SET %s WHERE id='%s';", table.concat(updates, ", "), task_id)
    end

    status = local_update(brain_file, update_statement)
    if status == nil then
        return nil, "Failed to update task priority"
    end
    return persist_tasks(brain_file)
end

function last_done(brain_file, args)
    subject = args["subject"] or ""
    num = args["number"] or 5

    query = "SELECT content, subject, comment FROM tasks WHERE done IS NOT NULL "
    if subject  !=  "" then
        query = query .. string.format("AND subject='%s'", escape_sql(subject))
    end
    
    query = query .. " ORDER BY done DESC "

    if num  !=  "" then
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
    is_help = false
    for _, a in ipairs(cmd_args) do
        if a == "--help" or a == "-h" then
            is_help = true
            break
        end
    end

    if not is_help then
        ensure_priority_columns(brain_file)
    end
    
    subcommand = cmd_args[1]
    if subcommand  !=  nil and string.sub(subcommand, 1, 1)  !=  "-" then
        valid_subs = {
            ["add"] = true,
            ["list"] = true,
            ["done"] = true,
            ["delay"] = true,
            ["prioritize"] = true,
            ["rank"] = true,
            ["last"] = true
        }
        if valid_subs[subcommand] == nil then
            print("Unknown subcommand: " .. subcommand)
            print("Available subcommands: add, list, done, delay, prioritize, rank, last")
            return "success"
        end
        table.insert(cmd_args, 1, "-d")
    else
        subcommand = nil
    end

    arg_string = """
        -d --do arg string false
        -s --subject arg string false
        -t --due_to arg string false
        -i --id arg string false
        -m --comment arg string false
        -c --content arg string false
        -n --number arg number false
        -o --owner arg string false
        -p --importance arg string false
        -u --urgency arg string false
    """

    help_string = help.get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)
    if args == nil then
        return "success"
    end

    status, err = nil, nil
    if args  !=  nil then
        if args["do"] == "add" then
            status, err = add_task(brain_file, args)
        elseif args["do"] == "list" then
            status, err = list_tasks(brain_file, args)
        elseif args["do"] == "done" then
            status, err = mark_done(brain_file, args)
        elseif args["do"] == "delay" then
            status, err = delay_due(brain_file, args)
        elseif args["do"] == "prioritize" or args["do"] == "rank" then
            status, err = update_priority(brain_file, args)
        elseif args["do"] == "last" then
            status, err = last_done(brain_file, args)
        elseif args["do"] == nil then
            status, err = add_task(brain_file, args)
        else
            print("Unknown subcommand: " .. args["do"])
            print("Available subcommands: add, list, done, delay, prioritize, rank, last")
            return "success" -- Help printed
        end
    end
    if status  !=  true then
        print(err or "Task command failed")
        return "error"
    end
    return "success"
end

task.do_task = do_task
task.add_task = add_task
task.list_tasks = list_tasks
task.mark_done = mark_done
task.delay_due = delay_due
task.update_priority = update_priority
task.last_done = last_done

if string.match(arg[0], "task.lua$")  !=  nil then
    do_task(get_brain_path(), arg)
else
    -- Export the module
    return task
end
