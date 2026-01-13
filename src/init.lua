-- Define a module table
init = {}

utils = require("utils")
argparse = require("argparse")
database = require("database")
local_update = database.local_update
local_query = database.local_query
lfs = require("lfs")
vault_to_sql = require("vault_to_sql").vault_to_sql
get_help_string = require("help").get_help_string

sql_init = """
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
"""

function build_config_dir(home_dir)
	config_dir = joinpath(home_dir, ".config")
	status = create_dir_if_not_exists(config_dir)
	if status == nil then
		return
	end

	bx_config_dir = joinpath(home_dir, ".config", "brain-ex")
	status = create_dir_if_not_exists(bx_config_dir)

	if status == nil then
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
    return (string.gsub(path, "/*$", ""))
end

function init_bx(args)
    brain_name = args["name"] or "brain"
    brain_name = remove_trailing_slash(brain_name)
    current_dir = lfs.currentdir()
    brain_path = current_dir .. "/" .. brain_name .. ".db"
    home_dir = os.getenv("HOME")
    default_editor = args["editor"] or "nano"

    -- remove old brain_path if it exists
    os.remove(brain_path)

    -- create database and tables
    success = local_update(brain_path, sql_init)
	if success == nil then
		print("Failed to initilize database")
		return
	end

    -- store info in ~/.config/brain-ex/config.yaml filr
    config_dir = build_config_dir(home_dir)
    config_file = joinpath(config_dir, "config.yaml")
    file = io.open(config_file, "w")
    io.write(file, "brain: " .. brain_path .. "\n")
    io.write(file, "editor: " .. default_editor)
    io.close(file)
    return "success"
end

function init_bx_with_vault(args)
    vault_dir = args["vault"]
    current_dir = lfs.currentdir()
    brain_name = args["name"] or args["vault"]
    brain_name = remove_trailing_slash(brain_name)
    brain_path = joinpath(current_dir, brain_name .. ".db")
    vault_path = joinpath(current_dir, vault_dir)
    home_dir = os.getenv("HOME")
    task_file = joinpath(vault_dir, "tasks.tsv")
    default_editor = args["editor"] or "nano"
    enable_git = args["git"] or false
	
    -- remove old brain_path if it exists
    os.remove(brain_path)
    
    -- create database and tables
    success = local_update(brain_path, sql_init)
	if success == nil then
		print("Failed to initialize database")
		return
	end

    -- optional: import existing tasks if available
	if file_exists(task_file) != nil and file_exists(task_file) then
    	import_delimited(brain_path, task_file, "tasks", "\t")    
	end

        -- ensure vault directory exists
    if lfs.attributes(vault_path, "mode") == nil then
        lfs.mkdir(vault_path)
    end

    -- if --git flag is used, initialize a Git repo if not present
    if enable_git != nil and enable_git then
        git_dir = joinpath(vault_path, ".git")
        mode = lfs.attributes(git_dir, "mode")
        if mode == nil then
            print("Initializing new git repository in " .. vault_path)
            os.execute(string.format("git init '%s' >/dev/null 2>&1", vault_path))
            os.execute(string.format("cd '%s' && git add . && git commit -m 'Initial commit' >/dev/null 2>&1", vault_path))
        else
            -- print("Vault is already a git repository")
        end
    end
	
    -- store info in ~/.config/brain-ex/config.yaml
    config_dir = build_config_dir(home_dir)
    config_file = joinpath(config_dir, "config.yaml")
    file = io.open(config_file, "w")
    io.write(file, "vault: " .. vault_path .. "\n")
    io.write(file, "brain: " .. brain_path .. "\n")
    io.write(file, "editor: " .. default_editor .. "\n")
    io.write(file, "git: " .. tostring(enable_git) .. "\n")
    io.close(file)

    -- import existing notes if any
    vault_to_sql(vault_path, brain_path)
    return "success"
end

function do_init(cmd_args)
    arg_string = """
        -n --name arg string false
        -v --vault arg string false
        -e --editor arg string false
        -g --git flag string false
    """

    help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)

    status = nil
    if args != nil then
        if args["vault"] != nil then
            status = init_bx_with_vault(args)
        else
            status = init_bx(args)
        end
    end
    if status != "success" then
        print("Init command failed")
    end
    return "success"
end

init.sql_init = sql_init
init.do_init = do_init

if string.match(arg[0], "init.lua$") != nil then
    do_init(arg)
else
    -- Export the module
    return init
end
