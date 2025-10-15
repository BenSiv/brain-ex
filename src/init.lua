-- Define a module table
local init = {}

require("utils").using("utils")
local lfs = require("lfs")
local vault_to_sql = require("vault_to_sql").vault_to_sql
local get_help_string = require("help").get_help_string

local sql_init = [[
PRAGMA foreign_keys = ON;

CREATE TABLE notes (
    time TIMESTAMP DEFAULT (datetime('now', 'localtime')),
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
    time TIMESTAMP DEFAULT (datetime('now', 'localtime')),
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

function remove_trailing_slash(path)
    -- if path is just "/" return as-is
    if path == "/" then
        return path
    end
    -- remove one or more trailing slashes
    return (path:gsub("/*$", ""))
end

function init_bx(args)
    local brain_name = args["name"] or "brain"
    brain_name = remove_trailing_slash(brain_name)
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
    return "success"
end

function init_bx_with_vault(args)
    local vault_dir = args["vault"]
    local current_dir = lfs.currentdir()
    local brain_name = args["name"] or args["vault"]
    brain_name = remove_trailing_slash(brain_name)
    local brain_path = joinpath(current_dir, brain_name .. ".db")
    local vault_path = joinpath(current_dir, vault_dir)
    local home_dir = os.getenv("HOME")
    local task_file = joinpath(vault_dir, "tasks.tsv")
    local default_editor = args["editor"] or "nano"
    local enable_git = args["git"] or false
	
    -- remove old brain_path if it exists
    os.remove(brain_path)
    
    -- create database and tables
    local success = local_update(brain_path, sql_init)
	if not success then
		print("Failed to initialize database")
		return
	end

    -- optional: import existing tasks if available
	if file_exists(task_file) then
    	import_delimited(brain_path, task_file, "tasks", "\t")    
	end

        -- ensure vault directory exists
    if not lfs.attributes(vault_path, "mode") then
        lfs.mkdir(vault_path)
    end

    -- if --git flag is used, initialize a Git repo if not present
    if enable_git then
        local git_dir = joinpath(vault_path, ".git")
        local mode = lfs.attributes(git_dir, "mode")
        if not mode then
            print("Initializing new git repository in " .. vault_path)
            os.execute(string.format("git init '%s' >/dev/null 2>&1", vault_path))
            os.execute(string.format("cd '%s' && git add . && git commit -m 'Initial commit' >/dev/null 2>&1", vault_path))
        else
            -- print("Vault is already a git repository")
        end
    end
	
    -- store info in ~/.config/brain-ex/config.yaml
    local config_dir = build_config_dir(home_dir)
    local config_file = joinpath(config_dir, "config.yaml")
    local file = io.open(config_file, "w")
    file:write("vault: " .. vault_path .. "\n")
    file:write("brain: " .. brain_path .. "\n")
    file:write("editor: " .. default_editor .. "\n")
    file:write("git: " .. tostring(enable_git) .. "\n")
    file:close()

    -- import existing notes if any
    vault_to_sql(vault_path, brain_path)
    return "success"
end

local function do_init()
    local arg_string = [[
        -n --name arg string false
        -v --vault arg string false
        -e --editor arg string false
        -g --git flag string false
    ]]

    local help_string = get_help_string(arg[0])
    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    local status
    if args then
        if args["vault"] then
            status = init_bx_with_vault(args)
        else
            status = init_bx(args)
        end
    end
    if status ~= "success" then
        print("Init command failed")
    end
    return "success"
end

init.sql_init = sql_init
init.do_init = do_init

if arg[0] == "init.lua" then
    do_init()
else
    -- Export the module
    return init
end
