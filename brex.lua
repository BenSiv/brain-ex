
utils = require("utils")
starts_with = utils.starts_with
database = require("database")
prettyprint = require("prettyprint")
dataframes = require("dataframes")
argparse = require("argparse")
paths = require("paths")
dates = require("dates")

package.path = "src/?.lua;" .. package.path
bx_utils = require("bx_utils")
config = require("config")
get_brain_path = config.get_brain_path
is_git = config.is_git

help = require("help")
get_help_string = help.get_help_string

init = require("init")
do_init = init.do_init

note = require("note")
do_note = note.do_note

task = require("task")
do_task = task.do_task

update = require("update")
do_update = update.do_update

sql = require("sql")
do_sql = sql.do_sql

git = require("git")
auto_update = git.auto_update

function main()
    command_funcs = {
        ["init"] = do_init,
        ["note"] = do_note,
        ["task"] = do_task,
        ["update"] = do_update,
        ["sql"] = do_sql
    }

    arg[-1] = "lua" -- for the executable

    -- Simple argument parsing to check for [brain_name]
    -- Potential commands
    valid_commands = {}
    for k,v in pairs(command_funcs) do
        valid_commands[k] = true
    end

    target_brain = nil
    command = arg[1]
    args_start = 2

    -- Check if first arg is a command or a brain name
    if command != nil and valid_commands[command] == nil and not starts_with(command, "-") then
        -- Assume it is a brain name
        target_brain = command
        command = arg[2]
        args_start = 3
        
        if command == "init" then
             print("Error: 'init' command does not accept a positional brain name. Use 'brex init --name <name>' instead.")
             os.exit(1)
        end
    end

    
    if command != nil and not starts_with(command, "-") then
        arg[0] = "brex " .. command
    else
        arg[0] = "brex"
    end

    help_string = get_help_string(arg[0])
    
    -- Collect remaining arguments into a clean list for subcommands
    cmd_args = {}
    -- arg[1] is the command (init, note, etc.)
    -- Arguments for the command start at arg[2]
    -- Collect remaining arguments into a clean list for subcommands
    cmd_args = {}
    -- Arguments for the command start at args_start
    for i = args_start, #arg do
        table.insert(cmd_args, arg[i])
    end
    cmd_args[0] = arg[0]
    
    if command == nil then
        print(help_string)
        return
    end

    func = command_funcs[command]
    if func == nil then
        print("'" .. command .. "' is not a valid command\n")
        print(help_string)
        return
    end
    
    status = nil
    if command == "init" then
        if target_brain != nil then
             table.insert(cmd_args, "--name")
             table.insert(cmd_args, target_brain)
        end
        status = func(cmd_args)
        if status != "success" then
            os.exit(1)
        end
        return
    end
    
    brain_file = get_brain_path(target_brain)
    if brain_file != nil then
        status = func(brain_file, cmd_args)
        if status != "success" then
            os.exit(1)
        end
    else
        if target_brain != nil then
            print("Error: Brain '" .. target_brain .. "' not configured.")
        else
            print("Error: Default brain not configured.")
        end
        os.exit(1)
    end

    if is_git() then
        auto_update()
    end
end

-- run program
main()
