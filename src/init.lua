-- Define a module table
local init = {}

local lfs = require("lfs")
local sqlite = require("sqlite3")

-- local function script_path()
--     local str = debug.getinfo(1, "S").source:sub(2)
--     return str:match("(.*/)")
-- end

local function script_path()
    local file_path = arg[0]
    local dir_path = match("(.*/)", file_path)
    return dir_path
end

function init_bx()
    -- get database name
    io.write("Brain name: ")
    local brain_name = io.read()
    local current_dir = lfs.currentdir()
    local brain_file = current_dir .. "/" .. brain_name .. ".bx"
    local home_dir = os.getenv("HOME")

    -- remove old brain_file if it exists
    -- os.remove(brain_file)

    -- read sql init commands
    local bx_path = script_path()
    local init_file = io.open(bx_path .. "init_brain.sql", "r")
    local sql_commands = init_file:read("*a")
    init_file:close()

    -- create database and tables
    local db = sqlite.open(brain_file)
    db:exec(sql_commands)
    db:close()

    -- store info in ~/.bx filr
    local dot_file = io.open(home_dir .. "/.bx", "w")
    dot_file:write(brain_file)
    dot_file:close()
end

init.init_bx = init_bx

-- Export the module
return init