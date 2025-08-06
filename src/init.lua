-- Define a module table
local init = {}

require("utils").using("utils")
local lfs = require("lfs")
local vault_to_sql = require("vault_to_sql").vault_to_sql
local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = get_parent_dir(script_path)

function build_config_dir(home_dir)
	local config_dir = joinpath(home_dir, ".config")
	local status = create_dir_if_not_exists(config_dir)
	if not status then
		return
	end

	local bx_config_dir = joinpath(home_dir, ".config", "brain-ex")
	status = create_dir_if_not_exists(bx_config_dir)

	if not status then
		return
	end
	
    return bx_config_dir
end

function init_bx()
    -- get database name
    -- io.write("Brain name: ")
    -- local brain_name = io.read()
    local brain_name = args["name"] or "brain"
    local current_dir = lfs.currentdir()
    local brain_path = current_dir .. "/" .. brain_name .. ".db"
    local home_dir = os.getenv("HOME")
    -- io.write("Default editor: ")
    -- local default_editor = io.read()
    local default_editor = args["editor"]

    -- remove old brain_path if it exists
    os.remove(brain_path)

    -- read sql init commands
    local init_file = joinpath(script_dir, "init_brain.sql")
    local sql_commands = read(init_file)

    -- create database and tables
    local_update(brain_path, sql_commands)

    -- store info in ~/.config/brain-ex/config.yaml filr
    local config_dir = build_config_dir(home_dir)
    local config_file = joinpath(config_dir, "config.yaml")
    local file = io.open(config_file, "w")
    file:write("brain: " .. brain_path .. "\n")
    file:write("editor: " .. default_editor)
    file:close()
end

function init_bx_with_vault(args)
    -- get database name
    -- io.write("Vault path: ")
    -- local vault_dir = io.read()
    local vault_dir = args["vault"]
    local current_dir = lfs.currentdir()
    local brain_name = args["name"] or args["vault"]
    local brain_path = joinpath(current_dir, brain_name .. ".db")
    local vault_path = joinpath(current_dir, vault_dir)
    local home_dir = os.getenv("HOME")
    local task_file = joinpath(vault_dir, "tasks.tsv")
    -- io.write("Default editor: ")
    -- local default_editor = io.read()
    local default_editor = args["editor"]
	
    -- remove old brain_path if it exists
    os.remove(brain_path)

    -- read sql init commands
    local init_file = joinpath(script_dir, "init_brain.sql")
    local sql_commands = read(init_file)

    -- create database and tables
    local_update(brain_path, sql_commands)

	if file_exists(task_file) then
    	import_delimited(brain_path, task_file, "tasks", "\t")    
	end
	
    -- store info in ~/.config/brain-ex/config.yaml filr
    local config_dir = build_config_dir(home_dir)
    local config_file = joinpath(config_dir, "config.yaml")
    file = io.open(config_file, "w")
    file:write("vault: " .. vault_path .. "\n")
    file:write("brain: " .. brain_path .. "\n")
    file:write("editor: " .. default_editor)
    file:close()

    vault_to_sql(vault_path, brain_path)
end

local function init_brex()
    local arg_string = [[
        -n --name arg string false
        -v --vault arg string false
        -e --editor arg string false
    ]]

    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args)

    if args["vault"] then
        init_bx_with_vault(args)
    else
        init_bx()
    end
end

init.init_brex = init_brex

if arg[0] == "init.lua" then
    init_brex()
else
    -- Export the module
    return init
end