package.path = "lua-utils/src/?.lua;" .. package.path
require("utils").using("utils")
using("prettyprint")
using("dataframes")
using("database")
using("argparse")
using("paths")
using("dates")

package.path = "src/?.lua;" .. package.path
local app_state = require("app_state")
local commands = {
    ["init"] = require("init"),
    ["note"] = require("note"),
    ["task"] = require("task"),
    ["update"] = require("update"),
    ["sql"] = require("sql")
}
local get_help_string = require("help").get_help_string

local function main()
    local command_funcs = {
        ["init"] = commands["init"].do_init,
        ["note"] = commands["note"].do_note,
        ["task"] = commands["task"].do_task,
        ["update"] = commands["update"].do_update,
        ["sql"] = commands["sql"].do_sql
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

    -- Handle init command specially (no state needed)
    if command == "init" then
        func()
        return
    end

    -- Create application state for other commands
    local state, err = app_state.create()
    if not state then
        print(err)
        return
    end

    -- Execute command with state
    local success = func(state)

    -- Cleanup
    app_state.cleanup(state)

    return success
end

-- run program
main()
