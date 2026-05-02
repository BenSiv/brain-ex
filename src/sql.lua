-- Define a module table
sql = {}

utils = require("utils")
argparse = require("argparse")
database = require("database")
local_query = database.local_query
config = require("config")
get_brain_path = config.get_brain_path
dataframes = require("dataframes")
view = dataframes.view
function sqlite_shell(brain_file)
    os.execute("sqlite3 -column -header " .. brain_file)
end

function sqlite_query(brain_file, query)
    results = local_query(brain_file, query)
    if results == nil then
        return nil
    end
    if arg[0] != nil and string.match(arg[0], "sql") != nil then
        view(results)
    end
    return results
end

function do_sql(brain_file, cmd_args)
    arg_string = """
        -d --do arg string false
        -q --query arg string false
    """

    help_string = get_help_string(arg[0])
    expected_args = def_args(arg_string)
    args = parse_args(cmd_args, expected_args, help_string)

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
