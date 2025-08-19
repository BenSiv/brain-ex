-- Define a module table
local init = {}

require("utils").using("utils")
local lfs = require("lfs")
local vault_to_sql = require("vault_to_sql").vault_to_sql
local get_help_string = require("help").get_help_string

local sql_init = [[
PRAGMA foreign_keys = ON;

CREATE TABLE notes (
    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    subject TEXT,
    title TEXT,
    content TEXT,
    PRIMARY KEY (title, subject)
);

CREATE TABLE connections (
    source_title TEXT NOT NULL,
    source_subject TEXT,
    target_title TEXT NOT NULL,
    target_subject TEXT,
    PRIMARY KEY (source_title, source_subject, target_title, target_subject)
);

CREATE TABLE tasks (
    id INTEGER PRIMARY KEY,
    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    content TEXT,
    subject TEXT,
    due_to TIMESTAMP,
    overdue INTEGER,
    done TIMESTAMP DEFAULT NULL,
    comment TEXT DEFAULT NULL
);
]]

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

function init_bx(args)
    local brain_name = args["name"] or "brain"
    local current_dir = lfs.currentdir()
    local brain_path = current_dir .. "/" .. brain_name .. ".db"
    local home_dir = os.getenv("HOME")
    local default_editor = args["editor"] or "nano"

    -- remove old brain_path if it exists
    os.remove(brain_path)

    -- create database and tables
    local success = local_update(brain_path, sql_init)
	if not success then
		print("Failed to initilize database")
		return
	end

    -- store info in ~/.config/brain-ex/config.yaml filr
    local config_dir = build_config_dir(home_dir)
    local config_file = joinpath(config_dir, "config.yaml")
    local file = io.open(config_file, "w")
    file:write("brain: " .. brain_path .. "\n")
    file:write("editor: " .. default_editor)
    file:close()
end

function init_bx_with_vault(args)
    local vault_dir = args["vault"]
    local current_dir = lfs.currentdir()
    local brain_name = args["name"] or args["vault"]
    local brain_path = joinpath(current_dir, brain_name .. ".db")
    local vault_path = joinpath(current_dir, vault_dir)
    local home_dir = os.getenv("HOME")
    local task_file = joinpath(vault_dir, "tasks.tsv")
    local default_editor = args["editor"] or "nano"
	
    -- remove old brain_path if it exists
    os.remove(brain_path)
    
    -- create database and tables
    local success = local_update(brain_path, sql_init)
	if not success then
		print("Failed to initilize database")
		return
	end

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

local function do_init()
    local arg_string = [[
        -n --name arg string false
        -v --vault arg string false
        -e --editor arg string false
    ]]

    local help_string = get_help_string(arg[0])
    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    if args then
        if args["vault"] then
            init_bx_with_vault(args)
        else
            init_bx(args)
        end
    end
end

init.sql_init = sql_init
init.do_init = do_init

if arg[0] == "init.lua" then
    do_init()
else
    -- Export the module
    return init
end
