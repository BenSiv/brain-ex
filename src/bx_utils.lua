-- Define a module table
local bx_utils = {}

require("utils").using("utils")
local config = require("config")
local database = require("database")

-- Legacy functions that delegate to new config module
function get_brain_file()
    return config.get_brain_file()
end

function get_vault_path()
    return config.get_vault_path()
end

function get_default_editor()
    return config.get_default_editor()
end

-- Updated utility functions that take state parameter
local function is_id_unique(state, table_name, target_id)
    local query = string.format("SELECT COUNT(*) FROM %s WHERE id = '%s';", table_name, target_id)
    local result = state.database.query(state.brain_file, query)
    if result and result[1] then
        return result[1]["COUNT(*)"] == 0
    end
    return false
end

function generate_id(state, table_name, desired_length, seed)
    desired_length = desired_length or 10
    seed = seed or os.time()
    math.randomseed(seed)

    local id = ""
    local id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(state, table_name, id)
    end

    return id
end

function is_timestamp(str)
    local pattern = "^%d%d%d%d%-%d%d%-%d%d %d%d:%d%d:%d%d$"
    local match = string.match(str, pattern)
    if match then
        return true
    else
        return false
    end
end

function is_sqlite_empty(state, table_name)
    local query = "SELECT COUNT(*) FROM " .. table_name .. ";"
    local result = state.database.query(state.brain_file, query)
    if result and result[1] then
        for _, element in pairs(result[1]) do
            return element == 0
        end
    end
    return true
end

-- Export legacy functions for compatibility
bx_utils.get_brain_file = get_brain_file
bx_utils.get_vault_path = get_vault_path
bx_utils.get_default_editor = get_default_editor

-- Export new state-based functions
bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp
bx_utils.is_sqlite_empty = is_sqlite_empty

-- Export the module
return bx_utils
