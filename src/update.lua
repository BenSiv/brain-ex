-- Define a module table
local update = {}

local vault_to_sql = require("vault_to_sql").vault_to_sql
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

local database = require("database")

local function update_note_from_file(note_path, brain_file)
	note_path = note_path or user.input("Note path: ")

	local title = "note"
	local group = ""
	local vault_path = get_vault_path()
	if vault_path then
		-- Extract group and title from the note path
		title = note_path:match("([^/]+)%.md$")
		group = note_path:match(".*/([^/]+)/[^/]+%.md$")
	else
		title = title or user.input("Title: ")
		group = group or user.input("Group: ")
	end

	-- Read content from the note file
	local content = read(note_path)
	if not content then
		print("Failed to read note: " .. note_path)
		return
	end

	-- Check if the note already exists
	local select_stmt = string.format([[
		SELECT id FROM notes
		WHERE group_name = '%s' AND title = '%s';
	]], group, title)

	local rows = database.local_query(brain_file, select_stmt)

	local stmt
	if rows and #rows > 0 then
		-- Note exists, update it using ID
		local note_id = rows[1].id
		stmt = string.format([[
			UPDATE notes
			SET content = '%s'
			WHERE id = %d;
		]], content, note_id)
	else
		-- Note does not exist, insert new
		stmt = string.format([[
			INSERT INTO notes (group_name, title, content)
			VALUES ('%s', '%s', '%s');
		]], group, title, content)
	end

	local success = database.local_update(brain_file, stmt)
	if not success then
		print("Failed to edit note in brain file")
		return
	end

	return "success"
end


update.update_from_vault = update_from_vault
update.update_note_from_file = update_note_from_file

-- Export the module
return update
