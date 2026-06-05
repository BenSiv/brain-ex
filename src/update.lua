-- Define a module table
update = {}

utils = require("utils")
argparse = require("argparse")
config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
lfs = require("lfs")
database = require("database")
knowledge_pool = require("knowledge_pool")
local_update = database.local_update
local_query = database.local_query
vault_to_sql = require("vault_to_sql").vault_to_sql
process_content = require("vault_to_sql").process_content
sql_init = require("init").sql_init
get_help_string = require("help").get_help_string

function read_raw(path)
    f = io.open(path, "r")
    if f == nil then return "" end
    content = io.read(f, "*all")
    io.close(f)
    return content
end

function escape_sql(str)
    return string.gsub(str or "", "'", "''")
end

function get_db_lock_path(brain_file)
    return brain_file .. ".lock"
end

function release_db_lock(lock_path)
    if lock_path != nil and lfs.attributes(lock_path, "mode") != nil then
        lfs.rmdir(lock_path)
    end
end

function acquire_db_lock(brain_file, timeout_seconds)
    timeout_seconds = timeout_seconds or 30
    lock_path = get_db_lock_path(brain_file)
    deadline = os.time() + timeout_seconds

    while true do
        status = lfs.mkdir(lock_path)
        if status == true then
            return lock_path
        end

        modified = lfs.attributes(lock_path, "modification")
        if modified != nil and os.time() - modified > timeout_seconds then
            lfs.rmdir(lock_path)
        end

        if os.time() >= deadline then
            return nil, "Failed to acquire brain lock: " .. lock_path
        end

        os.execute("sleep 0.1")
    end
end

function with_db_lock(brain_file, callback)
    lock_path, err = acquire_db_lock(brain_file)
    if lock_path == nil then
        return nil, err
    end

    ok, status, callback_err = pcall(callback)
    release_db_lock(lock_path)

    if ok != true then
        return nil, status
    end

    return status, callback_err
end

function escape_field(val)
    if val == nil or val == "NULL" then
        return "NULL"
    end
    return "'" .. string.gsub(val, "'", "''") .. "'"
end

function sync_tasks_from_vault(vault_path, brain_file, force)
    local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    tasks_dir = joinpath(vault_path, "tasks")
    lfs_mod = require("lfs")
    bx_utils = require("bx_utils")
    
    attr_dir = lfs_mod.attributes(tasks_dir)
    if attr_dir == nil or attr_dir.mode != "directory" then
        return true
    end

    file_list = bx_utils.find_markdown_files(tasks_dir) or {}
    seen_ids = {}
    
    for _, item in ipairs(file_list) do
        file = item.rel_path
        file_path = joinpath(tasks_dir, file)
        attr = lfs_mod.attributes(file_path)
        if attr != nil then
            current_mod = tostring(attr.modification)
            file_size = attr.size
            
            local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
            
            stored_mod = nil
            stored_size = nil
            
            res_mod = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='task_file_mod_%s';", file))
            if res_mod != nil and #res_mod > 0 then
                stored_mod = res_mod[1].value or res_mod[1][1]
            end
            
            res_size = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='task_file_size_%s';", file))
            if res_size != nil and #res_size > 0 then
                stored_size = res_size[1].value or res_size[1][1]
            end
            
            task_id = nil
            res_id = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='task_id_for_%s';", file))
            if res_id != nil and #res_id > 0 then
                task_id = res_id[1].value or res_id[1][1]
            end

            needs_parse = (force == true) or (stored_mod == nil) or (stored_mod != current_mod) or (stored_size == nil) or (tostring(stored_size) != tostring(file_size)) or (task_id == nil)
            
            if needs_parse then
                file_content = read_raw(file_path) or ""
                metadata, body = bx_utils.parse_frontmatter(file_content)
                
                id = metadata.id
                if id == nil or id == "" then
                    id = bx_utils.generate_id("tasks", nil, nil, brain_file)
                    metadata.id = id
                    
                    new_content = bx_utils.serialize_frontmatter(metadata) .. body
                    f = io.open(file_path, "w")
                    if f != nil then
                        io.write(f, new_content)
                        io.close(f)
                    end
                    fresh_attr = lfs_mod.attributes(file_path)
                    if fresh_attr != nil then
                        current_mod = tostring(fresh_attr.modification)
                        file_size = fresh_attr.size
                    end
                end
                
                task_id = id
                seen_ids[task_id] = true
                seen_ids[tostring(task_id)] = true
                num_id = tonumber(task_id)
                if num_id != nil then
                    seen_ids[num_id] = true
                end
                
                subject = item.dir_path != "" and item.dir_path or (metadata.subject or "NULL")
                due_to = metadata.due_to or "NULL"
                overdue = tonumber(metadata.overdue) or 0
                done = metadata.done or "NULL"
                comment = metadata.comment or "NULL"
                owner = metadata.owner or "NULL"
                importance = tonumber(metadata.importance) or 1
                urgency = tonumber(metadata.urgency) or 1
                    time_val = metadata.time or os.date("%Y-%m-%d %H:%M:%S")
                    
                    esc_subject = escape_field(subject)
                    esc_due_to = escape_field(due_to)
                    esc_done = escape_field(done)
                    esc_comment = escape_field(comment)
                    esc_owner = escape_field(owner)
                    esc_content = string.gsub(bx_utils.strip(body), "'", "''")
                    esc_time = "'" .. string.gsub(time_val, "'", "''") .. "'"
                    
                    check_task = local_query(brain_file, string.format("SELECT id FROM tasks WHERE id='%s';", task_id))
                    if check_task != nil and #check_task > 0 then
                        local_update(brain_file, string.format("""
                            UPDATE tasks SET time=%s, content='%s', subject=%s, due_to=%s, overdue='%s', done=%s, comment=%s, owner=%s, importance=%d, urgency=%d WHERE id='%s';
                        """, esc_time, esc_content, esc_subject, esc_due_to, tostring(overdue), esc_done, esc_comment, esc_owner, importance, urgency, task_id))
                    else
                        local_update(brain_file, string.format("""
                            INSERT INTO tasks (id, time, content, subject, due_to, overdue, done, comment, owner, importance, urgency)
                            VALUES ('%s', %s, '%s', %s, %s, '%s', %s, %s, %s, %d, %d);
                        """, task_id, esc_time, esc_content, esc_subject, esc_due_to, tostring(overdue), esc_done, esc_comment, esc_owner, importance, urgency))
                    end
                    
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_file_mod_%s', '%s');", file, current_mod))
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_file_size_%s', '%s');", file, tostring(file_size)))
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('task_id_for_%s', '%s');", file, task_id))
                else
                    seen_ids[task_id] = true
                    seen_ids[tostring(task_id)] = true
                    num_id = tonumber(task_id)
                    if num_id != nil then
                        seen_ids[num_id] = true
                    end
                end
            end
        end
    
    all_db_tasks = local_query(brain_file, "SELECT id FROM tasks;") or {}
    for _, task_row in ipairs(all_db_tasks) do
        id = task_row.id or task_row[1]
        if seen_ids[id] == nil and seen_ids[tostring(id)] == nil then
            local_update(brain_file, string.format("DELETE FROM tasks WHERE id='%s';", id))
        end
    end
    
    res_keys = local_query(brain_file, "SELECT key, value FROM sync_meta WHERE key LIKE 'task_id_for_%';") or {}
    for _, r in ipairs(res_keys) do
        k = r.key or r[1]
        v = r.value or r[2]
        if seen_ids[v] == nil then
            f_name = string.sub(k, 13)
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_id_for_%s';", f_name))
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_file_mod_%s';", f_name))
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_file_size_%s';", f_name))
        end
    end
    
    return true
end

function sync_sessions_from_vault(vault_path, brain_file, force)
    local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    sessions_dir = joinpath(vault_path, "agent_sessions")
    lfs_mod = require("lfs")
    bx_utils = require("bx_utils")
    
    attr_dir = lfs_mod.attributes(sessions_dir)
    if attr_dir == nil or attr_dir.mode != "directory" then
        return true
    end
    
    files = readdir(sessions_dir) or {}
    seen_ids = {}
    
    for _, file in ipairs(files) do
        if string.match(file, "%.md$") != nil then
            file_path = joinpath(sessions_dir, file)
            attr = lfs_mod.attributes(file_path)
            if attr != nil then
                current_mod = tostring(attr.modification)
                file_size = attr.size
                
                local_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
                
                stored_mod = nil
                stored_size = nil
                
                res_mod = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='session_file_mod_%s';", file))
                if res_mod != nil and #res_mod > 0 then
                    stored_mod = res_mod[1].value or res_mod[1][1]
                end
                
                res_size = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='session_file_size_%s';", file))
                if res_size != nil and #res_size > 0 then
                    stored_size = res_size[1].value or res_size[1][1]
                end
                
                sess_id = nil
                res_id = local_query(brain_file, string.format("SELECT value FROM sync_meta WHERE key='session_id_for_%s';", file))
                if res_id != nil and #res_id > 0 then
                    sess_id = res_id[1].value or res_id[1][1]
                end
                
                needs_parse = (force == true) or (stored_mod == nil) or (stored_mod != current_mod) or (stored_size == nil) or (tostring(stored_size) != tostring(file_size)) or (sess_id == nil)
                
                if needs_parse then
                    file_content = read_raw(file_path) or ""
                    metadata, body = bx_utils.parse_frontmatter(file_content)
                    
                    id = metadata.id
                    if id == nil or id == "" then
                        id = string.gsub(file, "%.md$", "")
                    end
                    
                    sess_id = id
                    seen_ids[sess_id] = true
                    
                    name = metadata.name or "Unnamed Session"
                    created_at = metadata.created_at or os.date("%Y-%m-%d %H:%M:%S")
                    updated_at = metadata.updated_at or os.date("%Y-%m-%d %H:%M:%S")
                    
                    esc_id = string.gsub(sess_id, "'", "''")
                    esc_name = string.gsub(name, "'", "''")
                    esc_created_at = string.gsub(created_at, "'", "''")
                    esc_updated_at = string.gsub(updated_at, "'", "''")
                    
                    check_sess = local_query(brain_file, string.format("SELECT id FROM agent_sessions WHERE id='%s';", esc_id))
                    if check_sess != nil and #check_sess > 0 then
                        local_update(brain_file, string.format("UPDATE agent_sessions SET name='%s', created_at='%s', updated_at='%s' WHERE id='%s';", esc_name, esc_created_at, esc_updated_at, esc_id))
                    else
                        local_update(brain_file, string.format("INSERT INTO agent_sessions (id, name, created_at, updated_at) VALUES ('%s', '%s', '%s', '%s');", esc_id, esc_name, esc_created_at, esc_updated_at))
                    end
                    
                    msgs = bx_utils.parse_session_body(body)
                    
                    local_update(brain_file, string.format("DELETE FROM agent_messages WHERE session_id='%s';", esc_id))
                    
                    for _, msg in ipairs(msgs) do
                        esc_role = string.gsub(msg.role, "'", "''")
                        esc_content = string.gsub(msg.content or "", "'", "''")
                        esc_msg_created = string.gsub(msg.created_at or created_at, "'", "''")
                        
                        esc_meta = "NULL"
                        if msg.metadata != nil and msg.metadata != "" then
                            esc_meta = "'" .. string.gsub(msg.metadata, "'", "''") .. "'"
                        end
                        
                        in_context_val = msg.in_context or 1
                        
                        local_update(brain_file, string.format("""
                            INSERT INTO agent_messages (session_id, role, content, metadata, in_context, created_at)
                            VALUES ('%s', '%s', '%s', %s, %d, '%s');
                        """, esc_id, esc_role, esc_content, esc_meta, in_context_val, esc_msg_created))
                    end
                    
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_file_mod_%s', '%s');", file, current_mod))
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_file_size_%s', '%s');", file, tostring(file_size)))
                    local_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('session_id_for_%s', '%s');", file, sess_id))
                else
                    seen_ids[sess_id] = true
                end
            end
        end
    end
    
    all_db_sessions = local_query(brain_file, "SELECT id FROM agent_sessions;") or {}
    for _, sess_row in ipairs(all_db_sessions) do
        id = sess_row.id or sess_row[1]
        if seen_ids[id] == nil and seen_ids[tostring(id)] == nil then
            local_update(brain_file, string.format("DELETE FROM agent_messages WHERE session_id='%s';", id))
            local_update(brain_file, string.format("DELETE FROM agent_sessions WHERE id='%s';", id))
        end
    end
    
    res_keys = local_query(brain_file, "SELECT key, value FROM sync_meta WHERE key LIKE 'session_id_for_%';") or {}
    for _, r in ipairs(res_keys) do
        k = r.key or r[1]
        v = r.value or r[2]
        if seen_ids[v] == nil and seen_ids[tostring(v)] == nil then
            f_name = string.sub(k, 16)
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_id_for_%s';", f_name))
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_file_mod_%s';", f_name))
            local_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_file_size_%s';", f_name))
        end
    end
    
    return true
end

function update_from_vault(brain_file, force)
    vault_path = get_vault_path()

    if brain_file != nil and vault_path != nil then
        return with_db_lock(brain_file, function()
            if force == true then
                print("Force rebuild: dropping existing data...")
                reset_sql = """
                    DROP TABLE IF EXISTS connections;
                    DROP TABLE IF EXISTS notes;
                    DROP TABLE IF EXISTS tasks;
                    DROP TABLE IF EXISTS agent_sessions;
                    DROP TABLE IF EXISTS agent_messages;
                    DROP TABLE IF EXISTS sync_meta;
                """
                local_update(brain_file, reset_sql)
            end

            -- Ensure tables exist
            status = local_update(brain_file, sql_init)
            knowledge_pool.ensure_table(brain_file)
            if status == nil then
                return nil, "Failed to ensure database tables"
            end

            -- vault_to_sql now handles incremental updates
            status = vault_to_sql(vault_path, brain_file)
            if status == nil then
                return nil, "Failed to update from vault"
            end
            
            knowledge_pool.sync_notes(brain_file)

            -- Sync tasks and agent sessions from markdown files
            sync_tasks_from_vault(vault_path, brain_file, force)
            sync_sessions_from_vault(vault_path, brain_file, force)

            return true
        end)
    end
end

function update_note_from_file(brain_file, note_path)
	note_path = note_path or user.input("Note path: ")

	title = "note"
	subject = ""
	vault_path = get_vault_path()

	if vault_path != nil then
		-- Extract subject and title from the note path
		title = string.match(note_path, "([^/]+)%.md$")
		subject = string.match(note_path, ".*/([^/]+)/[^/]+%.md$") or ""
	else
		title = user.input("Title: ")
		subject = user.input("Subject: ")
	end

	-- Read content from the note file
	content = utils.read(note_path)
	if content == nil then
		return nil, "Failed to read note: " .. note_path
	end

	attr = lfs.attributes(note_path)
	note_time = attr and os.date("%Y-%m-%d %H:%M:%S", attr.modification) or os.date("%Y-%m-%d %H:%M:%S")
	note_size = attr and attr.size or 0

	links = {}
	if content != "" then
		content, links = process_content(content)
        -- Ensure links is a table if nil returned
        links = links or {}
	end

	-- Escape single quotes for SQL
    content = content or ""
    content = string.gsub(content, "'", "''")

	return with_db_lock(brain_file, function()
		-- Check if the note already exists
		note_exists_query = string.format("""
			SELECT COUNT(*) AS num FROM notes
			WHERE subject = '%s' AND title = '%s'
		""", subject, title)
		
		num_rows = 0
		result = local_query(brain_file, note_exists_query)
		if result != nil then
			-- Handle both named and numeric column access
			num_rows = tonumber(result[1].num or result[1][1]) or 0
		end

		-- Construct INSERT or UPDATE statement
		stmt = nil
		if num_rows > 0 then
			stmt = string.format("""
				UPDATE notes
				SET content = '%s', time = '%s', size = %d
				WHERE subject = '%s' AND title = '%s';
			""", content, note_time, note_size, subject, title)
		else
			stmt = string.format("""
				INSERT INTO notes (subject, title, content, time, size)
				VALUES ('%s', '%s', '%s', '%s', %d);
			""", subject, title, content, note_time, note_size)
		end

		-- Execute the statement
		success = local_update(brain_file, stmt)
		if success == nil then
			return nil, "Failed to update note from file: " .. note_path
		end

		-- Clear existing connections for this note
        clear_links = string.format("DELETE FROM connections WHERE source_title = '%s' AND source_subject = '%s';", title, subject)
		success = local_update(brain_file, clear_links)
		if success == nil then
			return nil, "Failed to clear note links from file: " .. note_path
		end

		-- Insert updated links
        if #links > 0 then
            insert_links = "INSERT INTO connections (source_title, source_subject, target_title, target_subject) VALUES "
            for i, link in ipairs(links) do
                statement_value = string.format(
                    "('%s', '%s', '%s', '%s'), ",
                    escape_sql(title),
                    escape_sql(subject),
                    escape_sql(link.title),
                    escape_sql(link.subject or "")
                )
                insert_links = insert_links .. statement_value
            end
            insert_links = string.sub(insert_links, 1, -3) .. ";"
			success = local_update(brain_file, insert_links)
			if success == nil then
                return nil, "Failed to update note links from file: " .. note_path
            end
        end

        knowledge_pool.sync_notes(brain_file)

        print("Updated note: " .. note_path)
		return true
    end)
end

function do_update(brain_file, cmd_args)
    arg_string = """
        -f --file arg string false
        -c --force flag boolean false
    """

	help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)
    if args == nil then
        return "success"
    end

	status, err = true, nil
	if args != nil then
		if args["file"] != nil then
			status, err = update_note_from_file(brain_file, args["file"])
		else
			status, err = update_from_vault(brain_file, args["force"])
		end
	end
	if status != true then
		print(err or "Update command failed")
		return "error"
	end
	return "success"
end

update.update_note_from_file = update_note_from_file
update.update_from_vault = update_from_vault
update.do_update = do_update

if string.match(arg[0], "update.lua$") != nil then
    do_update(get_brain_path(), arg)
else
    -- Export the module
    return update
end
