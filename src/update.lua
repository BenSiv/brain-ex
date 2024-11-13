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
        local_update(brain_file, sql_commands)

        vault_to_sql(vault_path, brain_file)
    end

    if task_file then
        import_delimited(brain_file, task_file, "tasks", "\t")
    end
end

-- Export the module
return update
