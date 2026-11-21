-- Define a module table
update = {}

config = require("config")
get_brain_path = config.get_brain_path
get_vault_path = config.get_vault_path
lfs = require("lfs")
database = require("database")
db = database
knowledge_pool = require("knowledge_pool")
local_update = database.exec
local_query = database.query
vault_to_sql = require("vault_to_sql").vault_to_sql
process_content = require("vault_to_sql").process_content
sql_init = require("sql_schema").sql_init
get_help_string = require("help").get_help_string
bx_utils = require("bx_utils")
paths = require("paths")
task_mod = require("task")

function read_raw(path)
    f = io.open(path, "r")
    if f == nil then return "" end
    content = io.read(f, "*all")
    io.close(f)
    return content
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

-- Cheap stat-only fingerprint of every markdown file under the vault
-- (notes and tasks): path+mtime+size, no file content read.
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
    rows = database.query(brain_file, "SELECT value FROM sync_meta WHERE key='vault_fingerprint';")
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
    database.exec(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
    database.exec(brain_file, string.format("INSERT OR REPLACE INTO sync_meta (key, value) VALUES ('vault_fingerprint', '%s');", fingerprint))
end

function update_from_vault(brain_file, force)
    vault_path = get_vault_path()

    if brain_file != nil and vault_path != nil then
        return with_db_lock(brain_file, function()
            if force == true then
                print("Force rebuild: dropping existing data...")
                reset_sql = """
                    DROP TABLE IF EXISTS connections;
                    DROP TABLE IF EXISTS tasks;
                    DROP TABLE IF EXISTS notes;
                    DROP TABLE IF EXISTS tasks_legacy_backup;
                    DROP TABLE IF EXISTS sync_meta;
                """
                database.exec(brain_file, reset_sql)
            end

            -- Ensure tables exist
            status = database.exec(brain_file, sql_init)
            knowledge_pool.ensure_table(brain_file)
            if status == nil then
                return nil, "Failed to ensure database tables"
            end

            -- Skip the full resync below entirely if nothing on disk has
            -- changed since the last sync (cheap path+mtime+size sweep,
            -- no content read) -- this is what makes it safe to run this
            -- function unconditionally before every write command.
            database.exec(brain_file, "CREATE TABLE IF NOT EXISTS sync_meta (key TEXT PRIMARY KEY, value TEXT);")
            current_fingerprint = compute_vault_fingerprint(vault_path)
            if force != true and get_stored_vault_fingerprint(brain_file) == current_fingerprint then
                return true
            end

            -- vault_to_sql handles incremental updates for notes and tasks
            status = vault_to_sql(vault_path, brain_file)
            if status == nil then
                return nil, "Failed to update from vault"
            end

            knowledge_pool.sync_notes(brain_file)

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
		-- Extract subject and title from the note path, relative to
		-- the vault root -- matching against the full path directly
		-- would capture the vault folder's own name as "subject" for
		-- any root-level note (no subject), since ".*/(x)/y.md" matches
		-- just as well when x is the vault root itself as when it's a
		-- real subject subdirectory. note_path isn't always prefixed
		-- with vault_path as a literal string (callers pass paths
		-- relative to cwd, e.g. `update --file`, as well as the
		-- vault_path-prefixed paths note.lua's own sync builds), so
		-- anchor on the vault directory's own basename appearing as a
		-- path segment rather than requiring an exact prefix match.
		vault_basename = string.match(vault_path, "([^/]+)/?$")
		if vault_basename == nil then
			vault_basename = vault_path
		end
		relative_path = note_path
		anchor_start, anchor_end = string.find(note_path, "/" .. vault_basename .. "/", 1, true)
		if anchor_start != nil then
			relative_path = string.sub(note_path, anchor_end + 1)
		elseif string.sub(note_path, 1, string.len(vault_basename) + 1) == vault_basename .. "/" then
			relative_path = string.sub(note_path, string.len(vault_basename) + 2)
		end
		title = string.match(relative_path, "([^/]+)%.md$")
		subject_match = string.match(relative_path, "^([^/]+)/[^/]+%.md$")
		subject = ""
		if subject_match != nil then
			subject = subject_match
		end
	else
		io.write("Title: ")
		io.flush()
		title = io.read()
		io.write("Subject: ")
		io.flush()
		subject = io.read()
	end

	-- Read content from the note file
	raw_content = paths.read_file(note_path)
	if raw_content == nil then
		return nil, "Failed to read note: " .. note_path
	end

	-- Frontmatter is optional (only task-tracked notes have one); a
	-- plain note's raw_content passes through parse_frontmatter
	-- unchanged, same as vault_to_sql.lua's own note sync.
	metadata, content = bx_utils.parse_frontmatter(raw_content)

	-- A task-tracked file's frontmatter carries the note's real title
	-- verbatim (task.lua writes it there), unlike the filename, which
	-- is necessarily lossy for a title containing "/" -- get_note_paths
	-- sanitizes that to "-" so it doesn't get read as a path separator
	-- (see doc/unified-items-design.md). Deriving title from the
	-- filename alone would then never find the note's actual (slash-
	-- containing) row by subject+title, silently creating a duplicate
	-- under the hyphenated title instead of updating the real one.
	if metadata != nil and metadata.title != nil and metadata.title != "" then
		title = metadata.title
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

	if content == nil then
		content = ""
	end

	return with_db_lock(brain_file, function()
		-- subject/title/content are free text off the filesystem/
		-- frontmatter (see the title derivation above) and can contain
		-- a plain English apostrophe. db.quote/db.literal (luam/lib/
		-- db.lua) is the shared, already-upstream helper for this --
		-- the same one daat's own document.lua uses -- rather than a
		-- project-local escape reimplementation; every SQL literal
		-- built below goes through it inline, or a title as ordinary
		-- as "admin 'refresh snapshot' action" breaks these statements.
		-- Check if the note already exists
		note_exists_query = string.format("""
			SELECT id FROM notes
			WHERE subject = %s AND title = %s
		""", db.quote(subject), db.quote(title))

		existing_id = nil
		result = database.query(brain_file, note_exists_query)
		if result != nil and #result > 0 then
			existing_id = result[1].id
			if existing_id == nil then
				existing_id = result[1][1]
			end
		end

		-- Construct INSERT or UPDATE statement
		stmt = nil
		note_id = existing_id
		if existing_id != nil and existing_id != "" then
			stmt = string.format("""
				UPDATE notes
				SET content = %s, time = %s, size = %d
				WHERE subject = %s AND title = %s;
			""", db.quote(content), db.quote(note_time), note_size, db.quote(subject), db.quote(title))
		else
			note_id = bx_utils.generate_id("notes", nil, nil, brain_file)
			stmt = string.format("""
				INSERT INTO notes (id, subject, title, content, time, size)
				VALUES (%s, %s, %s, %s, %s, %d);
			""", db.quote(note_id), db.quote(subject), db.quote(title), db.quote(content), db.quote(note_time), note_size)
		end

		-- Execute the statement
		success = database.exec(brain_file, stmt)
		if success == nil then
			return nil, "Failed to update note from file: " .. note_path
		end

		if metadata != nil and metadata.is_task == "true" then
			tasks_stmt = require("vault_to_sql").tasks_upsert_sql(note_id, metadata)
			success = database.exec(brain_file, tasks_stmt)
			if success == nil then
				return nil, "Failed to update task tracking from file: " .. note_path
			end
		end

		-- Clear existing connections for this note
        clear_links = string.format("DELETE FROM connections WHERE source_title = %s AND source_subject = %s;", db.quote(title), db.quote(subject))
		success = database.exec(brain_file, clear_links)
		if success == nil then
			return nil, "Failed to clear note links from file: " .. note_path
		end

		-- Insert updated links
        if #links > 0 then
            insert_links = "INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES "
            for i, link in ipairs(links) do
                link_subject = ""
                if link.subject != nil then
                    link_subject = link.subject
                end
                statement_value = string.format(
                    "(%s, %s, %s, %s), ",
                    db.quote(title),
                    db.quote(subject),
                    db.quote(link.title),
                    db.quote(link_subject)
                )
                insert_links = insert_links .. statement_value
            end
            insert_links = string.sub(insert_links, 1, -3) .. ";"
			success = database.exec(brain_file, insert_links)
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
    flag_specs = {
        {short = "f", long = "file", kind = "string"},
        {short = "c", long = "force", kind = "boolean"},
    }

	help_string = get_help_string(arg[0])
    args = bx_utils.parse_cmd_args(cmd_args, flag_specs, help_string)
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
update.sync_sessions_from_vault = sync_sessions_from_vault

bx_utils.update_from_vault = update_from_vault
bx_utils.update_note_from_file = update_note_from_file

if string.match(arg[0], "update.lua$") != nil then
    do_update(get_brain_path(), arg)
else
    -- Export the module
    return update
end
