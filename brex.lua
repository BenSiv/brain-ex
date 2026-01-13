
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

    command = arg[1]
    
    if is command and not starts_with(command, "-") then
        arg[0] = "brex " .. command
    else
        arg[0] = "brex"
    end

    help_string = get_help_string(arg[0])
    
    -- Collect remaining arguments into a clean list for subcommands
    cmd_args = {}
    -- arg[1] is the command (init, note, etc.)
    -- Arguments for the command start at arg[2]
    for i = 2, #arg do
        table.insert(cmd_args, arg[i])
    end
    cmd_args[0] = arg[0]
    
    if not is command then
        print(help_string)
        return
    end

    func = command_funcs[command]
    if not is func then
        print("'" .. command .. "' is not a valid command\n")
        print(help_string)
        return
    end
    
    status = nil
    if command == "init" then
        status = func(cmd_args)
        if status != "success" then
            os.exit(1)
        end
        return
    end
    
    brain_file = get_brain_path()
    if is brain_file then
        status = func(brain_file, cmd_args)
        if status != "success" then
            os.exit(1)
        end
    else
        os.exit(1)
    end

    if is_git() then
        auto_update()
    end
end

-- run program
main()
