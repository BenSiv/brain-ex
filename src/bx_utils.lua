-- Define a module table
local bx_utils = {}

local sqlite = require("sqlite3")
local yaml = require("yaml")

local function get_dot_file()
    local home_dir = os.getenv("HOME")
    local dot_file_path = home_dir .. "/.bx"

    local dot_file, error_message = io.open(dot_file_path, "r")

    if not dot_file then
        -- File doesn't exist, create it
        local dot_file = io.open(dot_file_path, "w")
        if not dot_file then
            print("Error: Unable to create .bx file.")
            return nil
        end
        dot_file:close()
    end

    dot_file:close()
    return dot_file_path
end

function get_brain_file()
    local dot_file_path = get_dot_file()
    local dot_file = io.open(dot_file_path, "r")

    if not dot_file then
        print("Unable to open dot file")
    end

    local content = dot_file:read("*a")
    dot_file:close()

    local brain_file = nil
    if content then
        local data = yaml.load(content)
        if data then
            brain_file = data["brain"]
        end
    end

    return brain_file
end

function get_vault_path()
    local dot_file_path = get_dot_file()
    local dot_file = io.open(dot_file_path, "r")

    if not dot_file then
        print("Unable to open dot file")
    end

    local content = dot_file:read("*a")
    dot_file:close()
    
    local vault_dir = nil
    if content then
        local data = yaml.load(content)
        if data then
            vault_dir = data["vault"]
        end
    end

    return vault_dir
end

local function is_id_unique(table_name, target_id)
    local brain_file = get_brain_file()
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

bx_utils.get_brain_file = get_brain_file
bx_utils.get_vault_path = get_vault_path
bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp

-- Export the module
return bx_utils