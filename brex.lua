
require("utils")
using("database")
using("prettyprint")
using("dataframes")
using("argparse")
using("paths")
using("dates")

package.path = "src/?.lua;" .. package.path
using("bx_utils")
using("config")
using("help")
using("init")
using("note")
using("task")
using("update")
using("sql")
using("git")

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
    
    if command and not starts_with(command, "-") then
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
    
    if not command then
        print(help_string)
        return
    end

    func = command_funcs[command]
    if not func then
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
    if brain_file then
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
