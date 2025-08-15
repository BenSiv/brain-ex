-- Define a module table
local sql = {}

function sql.do_sql(state)
    local success = os.execute(string.format("sqlite3 '%s'", state.brain_file))
    if not success then
        print("Failed to open sqlite shell")
        return false
    end
    
    return true
end

-- Export the module
return sql
