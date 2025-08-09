require("utils").using("utils")
using("prettyprint")
using("dataframes")
using("database")
using("argparse")
using("paths")
using("dates")

script_path = debug.getinfo(1, "S").source:sub(2)
add_to_path(script_path, "src")

using("bx_utils")
using("init")
using("note")
using("task")
using("update")
using("sql")

local function print_help()
    local help_str = unescape_string(read("src/main_help.txt"))
    print(help_str)
end

local function main()
    local command_funcs = {
        ["init"] = do_init,
        ["note"] = do_note,
        ["task"] = do_task,
        ["update"] = do_update,
        ["sql"] = do_sql
    }

    arg[-1] = "lua" -- for the executable
    
    if length(arg) == 2 then
        print("Missing command")
        print_help()
        return
    end

    local command = arg[1]
    arg[0] = "brex " .. command
    
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
        print_help()
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
