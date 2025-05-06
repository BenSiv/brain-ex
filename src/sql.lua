-- Define a module table
local sql = {}

function sqlite_shell(brain_file)
    os.execute("sqlite3 -column -header " .. brain_file)
end

function sqlite_query(brain_file)
    local query = input("Query: ")
    local results = local_query(brain_file, query)
    if not results then
        return nil
    end
    view(results)
end

sql.sqlite_shell = sqlite_shell
sql.sqlite_query = sqlite_query

-- Export the module
return sql
