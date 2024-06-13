-- Define a module table
local init = {}

local lfs = require("lfs")
local sqlite = require("sqlite3")
local path = require("path")
local vault_to_sql = require("vault_to_sql").vault_to_sql

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
    local brain_path = current_dir .. "/" .. brain_name .. ".bx"
    local home_dir = os.getenv("HOME")

    -- remove old brain_path if it exists
    -- os.remove(brain_path)

    -- read sql init commands
    local bx_path = script_path()
    local init_file = io.open(bx_path .. "init_brain.sql", "r")
    local sql_commands = init_file:read("*a")
    init_file:close()

    -- create database and tables
    local db = sqlite.open(brain_path)
    db:exec(sql_commands)
    db:close()

    -- store info in ~/.bx filr
    local dot_file = io.open(home_dir .. "/.bx", "w")
    dot_file:write("brain: " .. brain_path)
    dot_file:close()
end

function init_bx_with_vault()
    -- get database name
    io.write("Vault path: ")
    local vault_dir = io.read()
    local current_dir = lfs.currentdir()
    local brain_file = vault_dir .. ".bx"
    local brain_path = path(current_dir, brain_file)
    local vault_path = path(current_dir, vault_dir)
    local home_dir = os.getenv("HOME")

    -- remove old brain_path if it exists
    -- os.remove(brain_path)

    -- read sql init commands
    local bx_path = script_path()
    local init_file = io.open(bx_path .. "init_brain.sql", "r")
    local sql_commands = init_file:read("*a")
    init_file:close()

    -- create database and tables
    local db = sqlite.open(brain_path)
    db:exec(sql_commands)
    db:close()

    vault_to_sql(vault_path, brain_path)

    -- store info in ~/.bx file
    local dot_file = io.open(home_dir .. "/.bx", "w")
    dot_file:write("vault: " .. vault_path .. "\n")
    dot_file:write("brain: " .. brain_path)
    dot_file:close()
end


init.init_bx = init_bx
init.init_bx_with_vault = init_bx_with_vault

-- Export the module
return init