-- Define a module table
local sql = {}

function sqlite_shell(brain_file)
    os.execute("sqlite3 -column -header " .. brain_file)
end

function sqlite_query(brain_file, query)
    local results = local_query(brain_file, query)
    if not results then
        return nil
    end
    view(results)
end

local function do_sql(brain_file)
    local arg_string = [[
        -d --do arg string false
        -q --query arg string false
    ]]

    local help_string = get_help_string(arg[0])
    local expected_args = def_args(arg_string)
    local args = parse_args(arg, expected_args, help_string)

    if args then
        if args["query"] then
            sqlite_query(brain_file, args["query"])
        else
            sqlite_shell(brain_file)
        end
    end
end

sql.do_sql = do_sql

if arg[0] == "sql.lua" then
    do_sql()
else
    -- Export the module
    return sql
end
