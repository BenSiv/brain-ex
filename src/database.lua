-- Define a module table
local database = {}

require("utils").using("utils")
local sqlite = require("sqlite3")

-- Simple connection cache
local connection_cache = {}

local function get_connection(brain_file)
    if not connection_cache[brain_file] then
        connection_cache[brain_file] = sqlite.open(brain_file)
    end
    return connection_cache[brain_file]
end

function database.execute(brain_file, query)
    local db = get_connection(brain_file)
    local success, result = pcall(function()
        return db:exec(query)
    end)
    
    if not success then
        print("Database error: " .. result)
        return false
    end
    
    return true
end

function database.query(brain_file, sql)
    local db = get_connection(brain_file)
    local results = {}
    
    local success, err = pcall(function()
        for row in db:nrows(sql) do
            table.insert(results, row)
        end
    end)
    
    if not success then
        print("Database query error: " .. err)
        return nil
    end
    
    return results
end

function database.close_connection(brain_file)
    if connection_cache[brain_file] then
        connection_cache[brain_file]:close()
        connection_cache[brain_file] = nil
    end
end

function database.close_all()
    for brain_file, conn in pairs(connection_cache) do
        conn:close()
    end
    connection_cache = {}
end

-- Export the module
return database
