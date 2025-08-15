-- Define a module table
local bx_utils = {}

require("utils").using("utils")
local sqlite = require("sqlite3")
local get_help_string = require("help").get_help_string

-- local function get_config_file()
--     local home_dir = os.getenv("HOME")
--     local config_file_path = joinpath(home_dir, ".config", "brain-ex", "config.yaml")

--     if file_exists(config_file_path) then
--         return config_file_path
--     else
--         print(string.format("Error: %s file do not exist, run brex init.", config_file_path))
--         print(get_help_string("brex"))
--         return nil
--     end
-- end

-- function get_brain_file()
--     local config_file_path = get_config_file()
--     local brain_file = nil
--     if config_file_path then
--         local content = read_yaml(config_file_path)
--         brain_file = content["brain"]
--     end
--     return brain_file
-- end

-- function get_vault_path()
--     local config_file_path = get_config_file()
--     local vault_dir = nil
--     if config_file_path then
--         local content = read_yaml(config_file_path)
--         local vault_dir = content["vault"]
--     end
--     return vault_dir
-- end

-- function get_default_editor()
--     local config_file_path = get_config_file()
--     local editor = nil
--     if config_file_path then
--         local content = read_yaml(config_file_path)
--         local editor = content["editor"]
--     end
--     return editor
-- end

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

-- bx_utils.get_brain_file = get_brain_file
-- bx_utils.get_vault_path = get_vault_path
-- bx_utils.get_default_editor = get_default_editor
bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp
bx_utils.is_sqlite_empty = is_sqlite_empty

-- Export the module
return bx_utils
