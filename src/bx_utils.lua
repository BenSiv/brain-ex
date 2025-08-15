-- Define a module table
local bx_utils = {}

require("utils").using("utils")
local sqlite = require("sqlite3")
local get_help_string = require("help").get_help_string

local function is_id_unique(table_name, target_id)
    local brain_file = get_brain_path()
    local query = string.format("SELECT COUNT(*) FROM %s WHERE id = '%s';", table_name, target_id)
    local db = sqlite.open(brain_file)
    local is_unique = nil
    local count = db:rows(query)()
    if count["COUNT(*)"] ~= 0 then
        is_unique = false
    else
        is_unique = true
    end
    db:close()
    return is_unique
end

function generate_id(table_name, desired_length, seed)
    desired_length = desired_length or 10
    seed = seed or os.time()
    math.randomseed(seed)

    local id = ""
    local id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(table_name, id)
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

function is_sqlite_empty(brain_file, table_name)
    local query = "SELECT COUNT(*) FROM " .. table_name .. ";"
    local db = sqlite.open(brain_file)
    local answer = false
    for row in db:rows(query) do
        for _ ,element in pairs(row) do
            answer = element == 0
       end
    end
    db:close()
    return answer
end

bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp
bx_utils.is_sqlite_empty = is_sqlite_empty

-- Export the module
return bx_utils
