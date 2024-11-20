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

-- Export the module
return update
