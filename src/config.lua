-- Define a module table
local config = {}

-- Module-level cache
local cached_config = nil
local config_path = nil

local function get_config_path()
    if not config_path then
        local home_dir = os.getenv("HOME")
        config_path = joinpath(home_dir, ".config", "brain-ex", "config.yaml")
    end
    return config_path
end

local function load_config()
    if cached_config then
        return cached_config
    end
    
    local path = get_config_path()
    if not file_exists(path) then
        print("Error: " .. path .. " file does not exist, run brex init.")
        return nil
    end
    
    cached_config = read_yaml(path)
    return cached_config
end

function config.get_brain_path()
    local cfg = load_config()
    return cfg and cfg["brain"]
end

function config.get_vault_path()
    local cfg = load_config()
    return cfg and cfg["vault"]
end

function config.get_default_editor()
    local cfg = load_config()
    return cfg and cfg["editor"]
end

function config.is_git()
    local cfg = load_config()
    return cfg and cfg["git"] or false
end

function config.reload()
    cached_config = nil
    return load_config()
end

-- Export the module
return config
