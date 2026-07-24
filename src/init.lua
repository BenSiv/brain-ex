-- Define a module table
init = {}

sql_schema = require("sql_schema")
sql_init = sql_schema.sql_init
init.sql_init = sql_init

utils = require("utils")
argparse = require("argparse")
database = require("database")
knowledge_pool = require("knowledge_pool")
local_update = database.sqlite_update
local_query = database.sqlite_query
lfs = require("lfs")
vault_to_sql = require("vault_to_sql").vault_to_sql
get_help_string = require("help").get_help_string
update_mod = require("update")
paths = require("paths")
task_mod = require("task")
agent_engine = require("agent_engine")

function build_config_dir(home_dir)
    config_root = paths.joinpath(home_dir, ".config")
    status = paths.create_dir_if_not_exists(config_root)
    if status == nil then
        return
    end

    bx_config_dir = paths.joinpath(config_root, "brain-ex")
    status = paths.create_dir_if_not_exists(bx_config_dir)
    if status == nil then
        return
    end

    return bx_config_dir
end

function save_config(path, conf)
    file = io.open(path, "w")
    if file == nil then
        return nil
    end
    if conf.brain  !=  nil then io.write(file, "brain: " .. conf.brain .. "\n") end
    if conf.editor  !=  nil then io.write(file, "editor: " .. conf.editor .. "\n") end
    if conf.vault  !=  nil then io.write(file, "vault: " .. conf.vault .. "\n") end
    if conf.git  !=  nil then io.write(file, "git: " .. tostring(conf.git) .. "\n") end
    if conf.hide_due_to != nil then io.write(file, "hide_due_to: " .. tostring(conf.hide_due_to) .. "\n") end
    
    if conf.brains  !=  nil then
        io.write(file, "brains:\n")
        for k,v in pairs(conf.brains) do
            io.write(file, "  " .. k .. ": " .. v .. "\n")
        end
    end
    io.close(file)
end

function update_config_file(home_dir, updates)
    config_dir = build_config_dir(home_dir)
    config_file = paths.joinpath(config_dir, "config.yaml")
    
    current_conf = {}
    f = io.open(config_file, "r")
    if f  !=  nil then
        io.close(f)
        current_conf = utils.read_yaml(config_file)
        if current_conf == nil then
            current_conf = {}
        end
    end
    
    -- Merge updates
    if updates.brains  !=  nil then
        if current_conf.brains == nil then current_conf.brains = {} end
        for k,v in pairs(updates.brains) do
            current_conf.brains[k] = v
        end
        updates.brains = nil -- handled
    end

    for k,v in pairs(updates) do
        current_conf[k] = v
    end
    
    save_config(config_file, current_conf)

    settings_file = paths.joinpath(config_dir, "settings.json")
    if not paths.file_exists(settings_file) then
        sf = io.open(settings_file, "w")
        if sf != nil then
            json_template = """{
  "task_columns": ["id", "priority", "subject", "content", "due_to"],
  "colors": {
    "Q1": "\u001b[31m",
    "Q2": "\u001b[38;5;208m",
    "Q3": "\u001b[33m",
    "Q4": "\u001b[90m",
    "reset": "\u001b[0m"
  }
}
"""
            io.write(sf, json_template)
            io.close(sf)
        end
    end
end

function remove_trailing_slash(path)
    -- if path is just "/" return as-is
    if path == "/" then
        return path
    end
    -- remove one or more trailing slashes
    return (string.gsub(path, "/*$", ""))
end

function get_path_label(path)
    path_arg = path
    if path_arg == nil then
        path_arg = ""
    end
    normalized = remove_trailing_slash(path_arg)
    if normalized == "" then
        return ""
    end
    -- Use the last path component as the label (e.g. /a/b/vault -> vault)
    label = string.match(normalized, "([^/]+)$")
    if label != nil then
        return label
    end
    return normalized
end

function init_bx(args)
    brain_name = "brain"
    if args["name"] != nil then
        brain_name = args["name"]
    end
    brain_name = remove_trailing_slash(brain_name)
    current_dir = lfs.currentdir()
    brain_path = current_dir .. "/" .. brain_name .. ".db"
    home_dir = os.getenv("HOME")
    default_editor = "nano"
    if args["editor"] != nil then
        default_editor = args["editor"]
    end

    -- remove old brain_path if it exists
    os.remove(brain_path)

    -- create database and tables
    success = database.sqlite_update(brain_path, sql_init)
    knowledge_pool.ensure_table(brain_path)
	if success == nil then
		return nil, "Failed to initialize database"
	end

    -- store info in ~/.config/brain-ex/config.yaml filr
    -- store info in ~/.config/brain-ex/config.yaml
    updates = {
        editor = default_editor
    }
    if brain_name == "brain" then
        updates.brain = brain_path
    else
        updates.brains = {}
        updates.brains[brain_name] = brain_path
    end
    update_config_file(home_dir, updates)
    return true
end

function init_bx_with_vault(args)
    vault_dir = remove_trailing_slash(args["vault"])
    current_dir = lfs.currentdir()
    vault_name = get_path_label(vault_dir)
    brain_name = vault_name
    if args["name"] != nil then
        brain_name = args["name"]
    end
    brain_name = remove_trailing_slash(brain_name)
    brain_path = paths.joinpath(current_dir, brain_name .. ".db")
    vault_path = paths.joinpath(current_dir, vault_dir)
    home_dir = os.getenv("HOME")
    task_file = paths.joinpath(vault_dir, "tasks.tsv")
    default_editor = "nano"
    if args["editor"] != nil then
        default_editor = args["editor"]
    end
    enable_git = false
    if args["git"] != nil then
        enable_git = args["git"]
    end
	
    -- remove old brain_path if it exists
    os.remove(brain_path)
    
    -- create database and tables
    success = database.sqlite_update(brain_path, sql_init)
    knowledge_pool.ensure_table(brain_path)
	if success == nil then
		return nil, "Failed to initialize database"
	end

    -- optional: import existing tasks if available and migrate to Markdown
    if paths.file_exists(task_file) != nil and paths.file_exists(task_file) then
        print("WARNING: TSV support is deprecated and will be removed in a future release. Migrating legacy tasks.tsv to Markdown...")
        database.import_delimited(brain_path, task_file, "tasks", "\t")    
        task_mod.backup_tasks(brain_path)
        os.remove(task_file)
    end

    sessions_file = paths.joinpath(vault_dir, "agent_sessions.tsv")
    messages_file = paths.joinpath(vault_dir, "agent_messages.tsv")
    if (paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file)) or (paths.file_exists(messages_file) != nil and paths.file_exists(messages_file)) then
        print("WARNING: TSV support is deprecated and will be removed in a future release. Migrating legacy agent sessions/messages to Markdown...")
        if paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file) then
            database.import_delimited(brain_path, sessions_file, "agent_sessions", "\t")
        end
        if paths.file_exists(messages_file) != nil and paths.file_exists(messages_file) then
            database.import_delimited(brain_path, messages_file, "agent_messages", "\t")
        end
        agent_engine.backup_agent_data(brain_path)
        if paths.file_exists(sessions_file) != nil and paths.file_exists(sessions_file) then os.remove(sessions_file) end
        if paths.file_exists(messages_file) != nil and paths.file_exists(messages_file) then os.remove(messages_file) end
    end

        -- ensure vault directory exists
    if lfs.attributes(vault_path, "mode") == nil then
        lfs.mkdir(vault_path)
    end

    -- if --git flag is used, initialize a Git repo if not present
    if enable_git  !=  nil and enable_git then
        git_dir = paths.joinpath(vault_path, ".git")
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
    -- store info in ~/.config/brain-ex/config.yaml
    updates = {
        vault = vault_path,
        editor = default_editor,
        git = enable_git
    }
    if brain_name == vault_name or brain_name == "brain" then
        updates.brain = brain_path
    end
    updates.brains = {}
    updates.brains[brain_name] = brain_path
    update_config_file(home_dir, updates)

    -- import existing notes, tasks and agent sessions if any
    vault_to_sql(vault_path, brain_path)
    knowledge_pool.sync_notes(brain_path)
    update_mod.sync_tasks_from_vault(vault_path, brain_path)
    update_mod.sync_sessions_from_vault(vault_path, brain_path)
    return true
end

function do_init(cmd_args)
    arg_string = """
        -n --name arg string false
        -v --vault arg string false
        -e --editor arg string false
        -g --git flag string false
    """

    help_string = get_help_string(arg[0])
    expected_args = argparse.def_args(arg_string)
    args = argparse.parse_args(cmd_args, expected_args, help_string)
    if args == nil then
        return "success"
    end

    status, err = nil, nil
    if args["vault"]  !=  nil then
        status, err = init_bx_with_vault(args)
    else
        status, err = init_bx(args)
    end

    if status == nil then
        error_message = "Init command failed"
        if err != nil then
            error_message = err
        end
        print(error_message)
        return "error"
    end
    return "success"
end

init.do_init = do_init

if string.match(arg[0], "init.lua$")  !=  nil then
    do_init(arg)
else
    -- Export the module
    return init
end
