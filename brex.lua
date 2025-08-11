
package.path = "lua-utils/src/?.lua;" .. package.path
require("utils").using("utils")
using("prettyprint")
using("dataframes")
using("database")
using("argparse")
using("paths")
using("dates")

package.path = "src/?.lua;" .. package.path
using("bx_utils")
using("init")
using("note")
using("task")
using("update")
using("sql")
using("help")

local function main()
    local command_funcs = {
        ["init"] = do_init,
        ["note"] = do_note,
        ["task"] = do_task,
        ["update"] = do_update,
        ["sql"] = do_sql
    }

    arg[-1] = "lua" -- for the executable

    local command = arg[1]
    
    if command and not starts_with(command, "-") then
        arg[0] = "brex " .. command
    else
        arg[0] = "brex"
    end

    local help_string = get_help_string(arg[0])
    
    if length(arg) == 2 then
        print("Missing command")
        print(help_string)
        return
    end
    
    if length(arg) == 3 then
        arg[1] = nil
    end

    if length(arg) > 3 then
        if not starts_with(arg[2], "-") then
            arg[0] = arg[0] .. " " .. arg[2]
            arg[1] = "--do"
        else
            for i,_ in pairs(arg) do
                if i > 0 then
                    arg[i] = arg[i+1]
                end
            end
        end
    end

    local func = command_funcs[command]
    if not func then
        print("'" .. command .. "' is not a valid command\n")
        print(help_string)
        return
    end

    local brain_file
    if command ~= "init" then
        brain_file = get_brain_file()
    end
    func(brain_file)
end

-- run program
main()
