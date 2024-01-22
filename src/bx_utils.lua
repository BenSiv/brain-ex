-- Define a module table
local bx_utils = {}

local sqlite = require("sqlite3")

local function get_brain_file()
	local home_dir = os.getenv("HOME")
    local dot_file = io.open(home_dir .. "/.bx", "r")
    local brain_file = dot_file:read("*a")
    return brain_file
end

-- local function shuffle(t)
--     local n = length(t)
--     for i = n, 2, -1 do
--         local j = math.random(i)
--         t[i], t[j] = t[j], t[i]
--     end
-- end

function is_timestamp(str)
    local pattern = "^%d{4}-%d{2}-%d{2} %d{2}:%d{2}:%d{2}$"
    local match = string.match(str, pattern)
    if match then
          return true
    else
       return false
    end
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

function generate_id(table_name, desired_length)
    desired_length = desired_length or 10
    local id = ""
    local id_unique = false
    while not id_unique do
        id = string.format("%0" .. desired_length .. "d", math.random(10 ^ (desired_length - 1)))
        id_unique = is_id_unique(table_name, id)
    end
    return id
end

-- bx_utils.get_brain_file = get_brain_file
bx_utils.generate_id = generate_id
bx_utils.is_timestamp = is_timestamp

-- Export the module
return bx_utils