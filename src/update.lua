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
local_update = database.sqlite_update
local_query = database.sqlite_query
vault_to_sql = require("vault_to_sql").vault_to_sql
process_content = require("vault_to_sql").process_content
sql_init = require("sql_schema").sql_init
get_help_string = require("help").get_help_string
bx_utils = require("bx_utils")
paths = require("paths")
task_mod = require("task")
agent_engine = require("agent_engine")

function read_raw(path)
    f = io.open(path, "r")
    if f == nil then return "" end
    content = io.read(f, "*all")
    io.close(f)
    return content
end

function escape_sql(str)
    if str == nil then
        str = ""
    end
    return string.gsub(str, "'", "''")
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
    if timeout_seconds == nil then
        timeout_seconds = 30
    end
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

-- Cheap stat-only fingerprint of every markdown file under the vault
-- (notes, tasks, agent_sessions): path+mtime+size, no file content read.
-- Lets update_from_vault skip the expensive parse/diff/resync of every
-- subsystem when nothing has changed on disk since the last sync.
function compute_vault_fingerprint(vault_path)
    file_list_raw = bx_utils.find_markdown_files(vault_path)
    file_list = {}
    if file_list_raw != nil then
        file_list = file_list_raw
    end

    entries = {}
    for _, item in ipairs(file_list) do
        full_path = paths.joinpath(vault_path, item.rel_path)
        attr = lfs.attributes(full_path)
        if attr != nil then
            table.insert(entries, item.rel_path .. ":" .. tostring(attr.modification) .. ":" .. tostring(attr.size))
        end
    end

    table.sort(entries)
    return knowledge_pool.content_hash(table.concat(entries, "|"))
end

function get_stored_vault_fingerprint(brain_file)
    rows = database.sqlite_query(brain_file, "SELECT value FROM sync_meta WHERE key='vault_fingerprint';")
    if rows == nil or #rows == 0 then
        return nil
    end
    value = rows[1][1]
    if rows[1].value != nil then
        value = rows[1].value
    end
    return value
end

function store_vault_fingerprint(brain_file, fingerprint)
    database.sqlite_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    database.sqlite_update(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('vault_fingerprint', '%s');", fingerprint))
end

function sync_tasks_from_vault(vault_path, brain_file, force)
    database.sqlite_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    tasks_dir = paths.joinpath(vault_path, "tasks")
    
    attr_dir = lfs.attributes(tasks_dir)
    if attr_dir == nil or attr_dir.mode != "directory" then
        return true
    end

    file_list_raw = bx_utils.find_markdown_files(tasks_dir)
    file_list = {}
    if file_list_raw != nil then
        file_list = file_list_raw
    end
    seen_ids = {}

    -- Perf: one query for every file's sync_meta row, up front, instead
    -- of 3 separate SELECTs per file inside the loop below -- with N
    -- task files that was 3N+ individual round-trips on every single
    -- task add/done/list call, regardless of whether anything actually
    -- changed (the exact same class of bug found and fixed the same
    -- day in platform-wip's own schema.sync_all, task #118 -- unrelated
    -- codebase, identical root cause: unconditional per-item DB
    -- round-trips instead of one batched read).
    stored_mod_by_file = {}
    stored_size_by_file = {}
    task_id_by_file = {}
    sync_meta_rows_raw = database.sqlite_query(brain_file,
        "SELECT key, value FROM sync_meta WHERE key LIKE 'task_file_mod_%' OR key LIKE 'task_file_size_%' OR key LIKE 'task_id_for_%';")
    sync_meta_rows = {}
    if sync_meta_rows_raw != nil then
        sync_meta_rows = sync_meta_rows_raw
    end
    for _, row in ipairs(sync_meta_rows) do
        k = row[1]
        if row.key != nil then
            k = row.key
        end
        v = row[2]
        if row.value != nil then
            v = row.value
        end
        if string.sub(k, 1, 14) == "task_file_mod_" then
            stored_mod_by_file[string.sub(k, 15)] = v
        elseif string.sub(k, 1, 15) == "task_file_size_" then
            stored_size_by_file[string.sub(k, 16)] = v
        elseif string.sub(k, 1, 12) == "task_id_for_" then
            task_id_by_file[string.sub(k, 13)] = v
        end
    end

    for _, item in ipairs(file_list) do
        file = item.rel_path
        file_path = paths.joinpath(tasks_dir, file)
        attr = lfs.attributes(file_path)
        if attr != nil then
            current_mod = tostring(attr.modification)
            file_size = attr.size

            stored_mod = stored_mod_by_file[file]
            stored_size = stored_size_by_file[file]
            task_id = task_id_by_file[file]

            needs_parse = (force == true) or (stored_mod == nil) or (stored_mod != current_mod) or (stored_size == nil) or (tostring(stored_size) != tostring(file_size)) or (task_id == nil)
            
            if needs_parse then
                file_content_raw = read_raw(file_path)
                file_content = ""
                if file_content_raw != nil then
                    file_content = file_content_raw
                end
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
                    fresh_attr = lfs.attributes(file_path)
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
                
                subject = "NULL"
                if metadata.subject != nil then
                    subject = metadata.subject
                end
                if item.dir_path != "" then
                    subject = item.dir_path
                end
                due_to = "NULL"
                if metadata.due_to != nil then
                    due_to = metadata.due_to
                end
                overdue_num = tonumber(metadata.overdue)
                overdue = 0
                if overdue_num != nil then
                    overdue = overdue_num
                end
                done = "NULL"
                if metadata.done != nil then
                    done = metadata.done
                end
                comment = "NULL"
                if metadata.comment != nil then
                    comment = metadata.comment
                end
                owner = "NULL"
                if metadata.owner != nil then
                    owner = metadata.owner
                end
                importance_num = tonumber(metadata.importance)
                importance = 1
                if importance_num != nil then
                    importance = importance_num
                end
                urgency_num = tonumber(metadata.urgency)
                urgency = 1
                if urgency_num != nil then
                    urgency = urgency_num
                end
                    time_val = metadata.time
                    if time_val == nil then
                        time_val = os.date("%Y-%m-%d %H:%M:%S")
                    end
                    
                    esc_subject = escape_field(subject)
                    esc_due_to = escape_field(due_to)
                    esc_done = escape_field(done)
                    esc_comment = escape_field(comment)
                    esc_owner = escape_field(owner)
                    esc_content = string.gsub(bx_utils.strip(body), "'", "''")
                    esc_time = "'" .. string.gsub(time_val, "'", "''") .. "'"
                    
                    check_task = database.sqlite_query(brain_file, string.format("SELECT id FROM tasks WHERE id='%s';", task_id))
                    if check_task != nil and #check_task > 0 then
                        database.sqlite_update(brain_file, string.format("""
                            UPDATE tasks SET time=%s, content='%s', subject=%s, due_to=%s, overdue='%s', done=%s, comment=%s, owner=%s, importance=%d, urgency=%d WHERE id='%s';
                        """, esc_time, esc_content, esc_subject, esc_due_to, tostring(overdue), esc_done, esc_comment, esc_owner, importance, urgency, task_id))
                    else
                        database.sqlite_update(brain_file, string.format("""
                            INSERT INTO tasks (id, time, content, subject, due_to, overdue, done, comment, owner, importance, urgency)
                            VALUES ('%s', %s, '%s', %s, %s, '%s', %s, %s, %s, %d, %d);
                        """, task_id, esc_time, esc_content, esc_subject, esc_due_to, tostring(overdue), esc_done, esc_comment, esc_owner, importance, urgency))
                    end
                    
                    database.sqlite_update(brain_file, string.format("""
                        INSERT OR REPLACE INTO sync_meta (key, value) VALUES
                            ('task_file_mod_%s', '%s'),
                            ('task_file_size_%s', '%s'),
                            ('task_id_for_%s', '%s');
                    """, file, current_mod, file, tostring(file_size), file, task_id))
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
    
    all_db_tasks_raw = database.sqlite_query(brain_file, "SELECT id FROM tasks;")
    all_db_tasks = {}
    if all_db_tasks_raw != nil then
        all_db_tasks = all_db_tasks_raw
    end
    for _, task_row in ipairs(all_db_tasks) do
        id = task_row[1]
        if task_row.id != nil then
            id = task_row.id
        end
        if seen_ids[id] == nil and seen_ids[tostring(id)] == nil then
            database.sqlite_update(brain_file, string.format("DELETE FROM tasks WHERE id='%s';", id))
        end
    end

    res_keys_raw = database.sqlite_query(brain_file, "SELECT key, value FROM sync_meta WHERE key LIKE 'task_id_for_%';")
    res_keys = {}
    if res_keys_raw != nil then
        res_keys = res_keys_raw
    end
    for _, r in ipairs(res_keys) do
        k = r[1]
        if r.key != nil then
            k = r.key
        end
        v = r[2]
        if r.value != nil then
            v = r.value
        end
        if seen_ids[v] == nil then
            f_name = string.sub(k, 13)
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_id_for_%s';", f_name))
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_file_mod_%s';", f_name))
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='task_file_size_%s';", f_name))
        end
    end
    
    return true
end

function sync_sessions_from_vault(vault_path, brain_file, force)
    database.sqlite_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    sessions_dir = paths.joinpath(vault_path, "agent_sessions")
    
    attr_dir = lfs.attributes(sessions_dir)
    if attr_dir == nil or attr_dir.mode != "directory" then
        return true
    end
    
    files_raw = utils.readdir(sessions_dir)
    files = {}
    if files_raw != nil then
        files = files_raw
    end
    seen_ids = {}

    -- Perf: same fix as sync_tasks_from_vault above -- one batched
    -- read of every session file's sync_meta row up front, instead of
    -- 3 separate SELECTs per file on every single write call.
    stored_mod_by_file = {}
    stored_size_by_file = {}
    sess_id_by_file = {}
    sync_meta_rows_raw = database.sqlite_query(brain_file,
        "SELECT key, value FROM sync_meta WHERE key LIKE 'session_file_mod_%' OR key LIKE 'session_file_size_%' OR key LIKE 'session_id_for_%';")
    sync_meta_rows = {}
    if sync_meta_rows_raw != nil then
        sync_meta_rows = sync_meta_rows_raw
    end
    for _, row in ipairs(sync_meta_rows) do
        k = row[1]
        if row.key != nil then
            k = row.key
        end
        v = row[2]
        if row.value != nil then
            v = row.value
        end
        if string.sub(k, 1, 17) == "session_file_mod_" then
            stored_mod_by_file[string.sub(k, 18)] = v
        elseif string.sub(k, 1, 18) == "session_file_size_" then
            stored_size_by_file[string.sub(k, 19)] = v
        elseif string.sub(k, 1, 15) == "session_id_for_" then
            sess_id_by_file[string.sub(k, 16)] = v
        end
    end

    for _, file in ipairs(files) do
        if string.match(file, "%.md$") != nil then
            file_path = paths.joinpath(sessions_dir, file)
            attr = lfs.attributes(file_path)
            if attr != nil then
                current_mod = tostring(attr.modification)
                file_size = attr.size

                stored_mod = stored_mod_by_file[file]
                stored_size = stored_size_by_file[file]
                sess_id = sess_id_by_file[file]

                needs_parse = (force == true) or (stored_mod == nil) or (stored_mod != current_mod) or (stored_size == nil) or (tostring(stored_size) != tostring(file_size)) or (sess_id == nil)
                
                if needs_parse then
                    file_content_raw = read_raw(file_path)
                    file_content = ""
                    if file_content_raw != nil then
                        file_content = file_content_raw
                    end
                    metadata, body = bx_utils.parse_frontmatter(file_content)

                    id = metadata.id
                    if id == nil or id == "" then
                        id = string.gsub(file, "%.md$", "")
                    end
                    
                    sess_id = id
                    seen_ids[sess_id] = true
                    
                    name = "Unnamed Session"
                    if metadata.name != nil then
                        name = metadata.name
                    end
                    created_at = metadata.created_at
                    if created_at == nil then
                        created_at = os.date("%Y-%m-%d %H:%M:%S")
                    end
                    updated_at = metadata.updated_at
                    if updated_at == nil then
                        updated_at = os.date("%Y-%m-%d %H:%M:%S")
                    end
                    
                    esc_id = string.gsub(sess_id, "'", "''")
                    esc_name = string.gsub(name, "'", "''")
                    esc_created_at = string.gsub(created_at, "'", "''")
                    esc_updated_at = string.gsub(updated_at, "'", "''")
                    
                    check_sess = database.sqlite_query(brain_file, string.format("SELECT id FROM agent_sessions WHERE id='%s';", esc_id))
                    if check_sess != nil and #check_sess > 0 then
                        database.sqlite_update(brain_file, string.format("UPDATE agent_sessions SET name='%s', created_at='%s', updated_at='%s' WHERE id='%s';", esc_name, esc_created_at, esc_updated_at, esc_id))
                    else
                        database.sqlite_update(brain_file, string.format("INSERT INTO agent_sessions (id, name, created_at, updated_at) VALUES ('%s', '%s', '%s', '%s');", esc_id, esc_name, esc_created_at, esc_updated_at))
                    end
                    
                    msgs = bx_utils.parse_session_body(body)
                    
                    database.sqlite_update(brain_file, string.format("DELETE FROM agent_messages WHERE session_id='%s';", esc_id))
                    
                    for _, msg in ipairs(msgs) do
                        esc_role = string.gsub(msg.role, "'", "''")
                        msg_content = ""
                        if msg.content != nil then
                            msg_content = msg.content
                        end
                        esc_content = string.gsub(msg_content, "'", "''")
                        msg_created = created_at
                        if msg.created_at != nil then
                            msg_created = msg.created_at
                        end
                        esc_msg_created = string.gsub(msg_created, "'", "''")

                        esc_meta = "NULL"
                        if msg.metadata != nil and msg.metadata != "" then
                            esc_meta = "'" .. string.gsub(msg.metadata, "'", "''") .. "'"
                        end

                        in_context_val = 1
                        if msg.in_context != nil then
                            in_context_val = msg.in_context
                        end
                        
                        database.sqlite_update(brain_file, string.format("""
                            INSERT INTO agent_messages (session_id, role, content, metadata, in_context, created_at)
                            VALUES ('%s', '%s', '%s', %s, %d, '%s');
                        """, esc_id, esc_role, esc_content, esc_meta, in_context_val, esc_msg_created))
                    end
                    
                    database.sqlite_update(brain_file, string.format("""
                        INSERT OR REPLACE INTO sync_meta (key, value) VALUES
                            ('session_file_mod_%s', '%s'),
                            ('session_file_size_%s', '%s'),
                            ('session_id_for_%s', '%s');
                    """, file, current_mod, file, tostring(file_size), file, sess_id))
                else
                    seen_ids[sess_id] = true
                end
            end
        end
    end
    
    all_db_sessions_raw = database.sqlite_query(brain_file, "SELECT id FROM agent_sessions;")
    all_db_sessions = {}
    if all_db_sessions_raw != nil then
        all_db_sessions = all_db_sessions_raw
    end
    for _, sess_row in ipairs(all_db_sessions) do
        id = sess_row[1]
        if sess_row.id != nil then
            id = sess_row.id
        end
        if seen_ids[id] == nil and seen_ids[tostring(id)] == nil then
            database.sqlite_update(brain_file, string.format("DELETE FROM agent_messages WHERE session_id='%s';", id))
            database.sqlite_update(brain_file, string.format("DELETE FROM agent_sessions WHERE id='%s';", id))
        end
    end

    res_keys_raw = database.sqlite_query(brain_file, "SELECT key, value FROM sync_meta WHERE key LIKE 'session_id_for_%';")
    res_keys = {}
    if res_keys_raw != nil then
        res_keys = res_keys_raw
    end
    for _, r in ipairs(res_keys) do
        k = r[1]
        if r.key != nil then
            k = r.key
        end
        v = r[2]
        if r.value != nil then
            v = r.value
        end
        if seen_ids[v] == nil and seen_ids[tostring(v)] == nil then
            f_name = string.sub(k, 16)
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_id_for_%s';", f_name))
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_file_mod_%s';", f_name))
            database.sqlite_update(brain_file, string.format("DELETE FROM sync_meta WHERE key='session_file_size_%s';", f_name))
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
                database.sqlite_update(brain_file, reset_sql)
            end

            -- Ensure tables exist
            status = database.sqlite_update(brain_file, sql_init)
            knowledge_pool.ensure_table(brain_file)
            if status == nil then
                return nil, "Failed to ensure database tables"
            end

            -- Migrate legacy TSVs if they exist
            task_file = paths.joinpath(vault_path, "tasks.tsv")
            sessions_file = paths.joinpath(vault_path, "agent_sessions.tsv")
            messages_file = paths.joinpath(vault_path, "agent_messages.tsv")
            
            if paths.file_exists(task_file) != nil and paths.file_exists(task_file) then
                print("WARNING: TSV support is deprecated and will be removed in a future release. Migrating legacy tasks.tsv to Markdown...")
                database.import_delimited(brain_file, task_file, "tasks", "\t")    
                task_mod.backup_tasks(brain_file)
                os.remove(task_file)
            end

            if (paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file)) or (paths.file_exists(messages_file) != nil and paths.file_exists(messages_file)) then
                print("WARNING: TSV support is deprecated and will be removed in a future release. Migrating legacy agent sessions/messages to Markdown...")
                if paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file) then
                    database.import_delimited(brain_file, sessions_file, "agent_sessions", "\t")
                end
                if paths.file_exists(messages_file) != nil and paths.file_exists(messages_file) then
                    database.import_delimited(brain_file, messages_file, "agent_messages", "\t")
                end
                agent_engine.backup_agent_data(brain_file)
                if paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file) then os.remove(sessions_file) end
                if paths.file_exists(messages_file) != nil and paths.file_exists(messages_file) then os.remove(messages_file) end
            end

            -- Skip the full resync below entirely if nothing on disk has
            -- changed since the last sync (cheap path+mtime+size sweep,
            -- no content read) -- this is what makes it safe to run this
            -- function unconditionally before every write command.
            database.sqlite_update(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
            current_fingerprint = compute_vault_fingerprint(vault_path)
            if force != true and get_stored_vault_fingerprint(brain_file) == current_fingerprint then
                return true
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

            store_vault_fingerprint(brain_file, compute_vault_fingerprint(vault_path))
            return true
        end)
    end
end

function update_note_from_file(brain_file, note_path)
	if note_path == nil then
		note_path = user.input("Note path: ")
	end

	title = "note"
	subject = ""
	vault_path = get_vault_path()

	if vault_path != nil then
		-- Extract subject and title from the note path
		title = string.match(note_path, "([^/]+)%.md$")
		subject_match = string.match(note_path, ".*/([^/]+)/[^/]+%.md$")
		subject = ""
		if subject_match != nil then
			subject = subject_match
		end
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
	note_time = os.date("%Y-%m-%d %H:%M:%S")
	if attr != nil then
		note_time = os.date("%Y-%m-%d %H:%M:%S", attr.modification)
	end
	note_size = 0
	if attr != nil then
		note_size = attr.size
	end

	links = {}
	if content != "" then
		content, links = process_content(content)
        -- Ensure links is a table if nil returned
        if links == nil then
            links = {}
        end
	end

	-- Escape single quotes for SQL
    if content == nil then
        content = ""
    end
    content = string.gsub(content, "'", "''")

	return with_db_lock(brain_file, function()
		-- Check if the note already exists
		note_exists_query = string.format("""
			SELECT COUNT(*) AS num FROM notes
			WHERE subject = '%s' AND title = '%s'
		""", subject, title)
		
		num_rows = 0
		result = database.sqlite_query(brain_file, note_exists_query)
		if result != nil then
			-- Handle both named and numeric column access
			num_field = result[1][1]
			if result[1].num != nil then
				num_field = result[1].num
			end
			num_num = tonumber(num_field)
			num_rows = 0
			if num_num != nil then
				num_rows = num_num
			end
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
		success = database.sqlite_update(brain_file, stmt)
		if success == nil then
			return nil, "Failed to update note from file: " .. note_path
		end

		-- Clear existing connections for this note
        clear_links = string.format("DELETE FROM connections WHERE source_title = '%s' AND source_subject = '%s';", title, subject)
		success = database.sqlite_update(brain_file, clear_links)
		if success == nil then
			return nil, "Failed to clear note links from file: " .. note_path
		end

		-- Insert updated links
        if #links > 0 then
            insert_links = "INSERT INTO connections (source_title, source_subject, target_title, target_subject) VALUES "
            for i, link in ipairs(links) do
                link_subject = ""
                if link.subject != nil then
                    link_subject = link.subject
                end
                statement_value = string.format(
                    "('%s', '%s', '%s', '%s'), ",
                    escape_sql(title),
                    escape_sql(subject),
                    escape_sql(link.title),
                    escape_sql(link_subject)
                )
                insert_links = insert_links .. statement_value
            end
            insert_links = string.sub(insert_links, 1, -3) .. ";"
			success = database.sqlite_update(brain_file, insert_links)
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
    expected_args = argparse.def_args(arg_string)
    args = argparse.parse_args(cmd_args, expected_args, help_string)
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
		err_msg = "Update command failed"
		if err != nil then
			err_msg = err
		end
		print(err_msg)
		return "error"
	end
	return "success"
end

update.update_note_from_file = update_note_from_file
update.update_from_vault = update_from_vault
update.do_update = do_update
update.sync_tasks_from_vault = sync_tasks_from_vault
update.sync_sessions_from_vault = sync_sessions_from_vault

bx_utils.update_from_vault = update_from_vault
bx_utils.update_note_from_file = update_note_from_file

if string.match(arg[0], "update.lua$") != nil then
    do_update(get_brain_path(), arg)
else
    -- Export the module
    return update
end
