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

function update_from_vault(brain_file)
    vault_path = get_vault_path()
    task_file = joinpath(vault_path, "tasks.tsv")
    reset_sql = """
    DROP TABLE IF EXISTS connections;
    DROP TABLE IF EXISTS notes;
    DROP TABLE IF EXISTS tasks;
    """

    if brain_file != nil and vault_path != nil then
        return with_db_lock(brain_file, function()
            status = local_update(brain_file, reset_sql)
            if status == nil then
                return nil, "Failed to reset database"
            end

            status = local_update(brain_file, sql_init)
            knowledge_pool.ensure_table(brain_file)
            if status == nil then
                return nil, "Failed to update database"
            end

            status = vault_to_sql(vault_path, brain_file)
            if status == nil then
                return nil, "Failed to update from vault"
            end
            knowledge_pool.sync_notes(brain_file)

            if file_exists(task_file) then
                status = import_delimited(brain_file, task_file, "tasks", "\t")
                if status == nil then
                    return nil, "Failed to import tasks"
                end
            end
            return true
        end)
    end

    return true
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
				SET content = '%s'
				WHERE subject = '%s' AND title = '%s';
			""", content, subject, title)
		else
			stmt = string.format("""
				INSERT INTO notes (subject, title, content)
				VALUES ('%s', '%s', '%s');
			""", subject, title, content)
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
    """

	help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)

	status, err = true, nil
	if args != nil then
		if args["file"] != nil then
			status, err = update_note_from_file(brain_file, args["file"])
		else
			status, err = update_from_vault(brain_file)
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
