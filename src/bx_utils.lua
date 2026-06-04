-- Define a module table
bx_utils = {}

database = require("database")
config = require("config")
get_brain_path = config.get_brain_path

function is_id_unique(table_name, target_id, brain_file)
    brain_file = brain_file or get_brain_path()
    query = string.format("SELECT COUNT(*) AS cnt FROM %s WHERE id = '%s';", table_name, target_id)
    res = database.local_query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    count_val = tonumber(res[1].cnt or res[1][1]) or 0
    return count_val == 0
end

function generate_id(table_name, desired_length, seed, brain_file)
    desired_length = desired_length or 10
    seed = seed or os.time()
    math.randomseed(seed)

    id = ""
    id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(table_name, id, brain_file)
    end

    return id
end

function is_timestamp(str)
    pattern = "^%d%d%d%d%-%d%d%-%d%d %d%d:%d%d:%d%d$"
    match = string.match(str, pattern)
    if match then
        return true
    else
        return false
    end
end

function is_sqlite_empty(brain_file, table_name)
    query = "SELECT COUNT(*) AS cnt FROM " .. table_name .. ";"
    res = database.local_query(brain_file, query)
    if res == nil or #res == 0 then
        return true
    end
    count_val = tonumber(res[1].cnt or res[1][1]) or 0
    return count_val == 0
end

bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp

-- Export the module
return bx_utils
