-- Define a module table
sql = {}

bx_utils = require("bx_utils")
database = require("database")
local_query = database.query
config = require("config")
get_brain_path = config.get_brain_path
dataframes = require("dataframes")
help = require("help")
view = dataframes.view
function sqlite_shell(brain_file)
    os.execute("sqlite3 -column -header " .. brain_file)
end

function sqlite_query(brain_file, query)
    results = database.query(brain_file, query)
    if results == nil then
        return nil
    end
    if arg[0] != nil and string.match(arg[0], "sql") != nil then
        view(results)
    end
    return results
end

function do_sql(brain_file, cmd_args)
    flag_specs = {
        {short = "d", long = "do", kind = "string"},
        {short = "q", long = "query", kind = "string"},
    }

    help_string = help.get_help_string(arg[0])
    args = bx_utils.parse_cmd_args(cmd_args, flag_specs, help_string)

    if args  !=  nil then
        if args["query"]  !=  nil then
            sqlite_query(brain_file, args["query"])
        else
            sqlite_shell(brain_file)
        end
    end
    return "success"
end

sql.do_sql = do_sql
sql.sqlite_query = sqlite_query

if string.match(arg[0], "sql.lua$")  !=  nil then
    do_sql(get_brain_path(), arg)
else
    -- Export the module
    return sql
end
