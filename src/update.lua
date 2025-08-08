-- Define a module table
local update = {}

local database = require("database")
local vault_to_sql = require("vault_to_sql").vault_to_sql
local process_content = require("vault_to_sql").process_content
local get_vault_path = require("bx_utils").get_vault_path
local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = get_parent_dir(script_path)

function update_from_vault(brain_file)
    local vault_path = get_vault_path()
    local task_file = joinpath(vault_path, "tasks.tsv")

    if brain_file and vault_path then
        os.remove(brain_file)

        -- read sql init commands
        local init_file = joinpath(script_dir, "init_brain.sql")
        local sql_commands = read(init_file)

        -- create database and tables
        local status = local_update(brain_file, sql_commands)
        if not status then
            print("Failed to update database")
            return nil
        end

        local status = vault_to_sql(vault_path, brain_file)
        if not status then
            print("Failed to update from vault")
            return nil
        end
    end

    if file_exists(task_file) then
        local status = import_delimited(brain_file, task_file, "tasks", "\t")
        if not status then
            print("Failed to import tasks")
            return nil
        end
    end
    return "success"
end

local function update_note_from_file(brain_file, note_path)
	note_path = note_path or user.input("Note path: ")

	local title = "note"
	local subject = ""
	local vault_path = get_vault_path()

	if vault_path then
		-- Extract subject and title from the note path
		title = note_path:match("([^/]+)%.md$")
		subject = note_path:match(".*/([^/]+)/[^/]+%.md$")
	else
		title = user.input("Title: ")
		subject = user.input("Subject: ")
	end

	-- Read content from the note file
	local content = read(note_path)
	if not content then
		print("Failed to read note: " .. note_path)
		return
	end

	content, links = process_content(content)

	-- Escape single quotes for SQL
    content = content:gsub("'", "''")

	-- Check if the note already exists
	local note_exists_query = string.format([[
		SELECT COUNT(*) AS num FROM notes
		WHERE subject = '%s' AND name = '%s'
	]], subject, title)
	
	local num_rows = 0
	local result = local_query(brain_file, note_exists_query)
	if result then
		num_rows = tonumber(result[1].num)
	end

	-- Construct INSERT or UPDATE statement
	local stmt
	if num_rows > 0 then
		stmt = string.format([[
			UPDATE notes
			SET content = '%s'
			WHERE subject = '%s' AND name = '%s';
		]], content, subject, title)
	else
		stmt = string.format([[
			INSERT INTO notes (subject, name, content)
			VALUES ('%s', '%s', '%s');
		]], subject, title, content)
	end

	-- Execute the statement
	local success = local_update(brain_file, stmt)
	if not success then
		print("Failed to update note from file: " .. note_path)
		return
	end

	-- Clear existing connections for this note
    local clear_links = string.format("DELETE FROM connections WHERE source = '%s';", title)
	success = local_update(brain_file, clear_links)
	if not success then
		print("Failed to clear note links from file: " .. note_path)
		return
	end

	-- Insert updated links
    if #links > 0 then
        local insert_links = "INSERT INTO connections (source, target) VALUES "
        for i, link in ipairs(links) do
            insert_links = insert_links .. string.format("('%s', '%s')%s", title, link, i < #links and "," or ";")
        end
		success = local_update(brain_file, insert_links)
		if not success then
			print("Failed to update note links from file: " .. note_path)
			return
		end
    end



	return "success"
end

local function do_update(brain_file)
    local arg_string = [[
        -f --file arg string false
    ]]

	local help_string = [[
		Description:
		Update the brain database from the vault.
		By default, this command will reinitialize the database and import all notes and tasks from the vault.

		Options:
		-f --file <note_path>  Update a specific note from the given file path instead of the entire vault.
		
		Examples:
		brex update
		brex update --file /path/to/vault/subject/note.md
	]]

    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

	if args then
		if args["file"] then
			update_note_from_file(brain_file, args["file"])
		else
			update_from_vault(brain_file)
		end
	end
end

update.update_note_from_file = update_note_from_file
update.do_update = do_update

if arg[0] == "update.lua" then
    do_update()
else
    -- Export the module
    return update
end