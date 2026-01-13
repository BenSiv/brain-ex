-- Define a module table
bx_utils = {}

sqlite = require("sqlite3")
config = require("config")
get_brain_path = config.get_brain_path

function is_id_unique(table_name, target_id)
    brain_file = get_brain_path()
    query = string.format("SELECT COUNT(*) FROM %s WHERE id = '%s';", table_name, target_id)
    db = sqlite.open(brain_file)
    is_unique = nil
    
    count_val = 0
    for row in sqlite.rows(db, query) do
        -- count_val = row["COUNT(*)"] or row[1] -- row might be indexed or keyed
        for _, v in pairs(row) do count_val = v break end 
    end
    
    if count_val != 0 then
        is_unique = false
    else
        is_unique = true
    end
    sqlite.close(db)
    return is_unique
end

function generate_id(table_name, desired_length, seed)
    desired_length = desired_length or 10
    seed = seed or os.time()
    math.randomseed(seed)

    id = ""
    id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(table_name, id)
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
    query = "SELECT COUNT(*) FROM " .. table_name .. ";"
    db = sqlite.open(brain_file)
    answer = false
    for row in sqlite.rows(db, query) do
        for _ ,element in pairs(row) do
            answer = element == 0
       end
    end
    sqlite.close(db)
    return answer
end

bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp

-- Export the module
return bx_utils
