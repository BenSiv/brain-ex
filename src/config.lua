-- Define a module table
config = {}

-- Explicit imports
paths = require("paths")
joinpath = paths.joinpath
utils = require("utils")
file_exists = paths.file_exists
read_yaml = utils.read_yaml

-- Module-level cache

-- Module-level cache
cached_config = nil
config_path = nil

function get_config_path()
    if not is config_path then
        home_dir = os.getenv("HOME")
        config_path = joinpath(home_dir, ".config", "brain-ex", "config.yaml")
    end
    return config_path
end

function load_config()
    if is cached_config then
        return cached_config
    end
    
    path = get_config_path()
    if not file_exists(path) then
        print("Error: " .. path .. " file does not exist, run brex init.")
        return nil
    end
    
    cached_config = read_yaml(path)
    return cached_config
end

function config.get_brain_path()
    cfg = load_config()
    return cfg and cfg["brain"]
end

function config.get_vault_path()
    cfg = load_config()
    return cfg and cfg["vault"]
end

function config.get_default_editor()
    cfg = load_config()
    return cfg and cfg["editor"]
end

function config.is_git()
    cfg = load_config()
    val = cfg and cfg["git"]
    if is val and (val == true or val == "true") then
        return true
    else
        return false
    end
end

function config.reload()
    cached_config = nil
    return load_config()
end

-- Export the module
return config
