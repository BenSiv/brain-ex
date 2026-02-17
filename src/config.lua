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
    if config_path == nil then
        home_dir = os.getenv("HOME")
        config_path = joinpath(home_dir, ".config", "brain-ex", "config.yaml")
    end
    return config_path
end

function load_config()
    if cached_config != nil then
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

function get_path_label(path)
    if path == nil or path == "" then
        return nil
    end
    normalized = path
    if path != "/" then
        normalized = string.gsub(path, "/*$", "")
    end
    label = string.match(normalized, "([^/]+)$")
    return label or normalized
end

function save_config_file(path, conf)
    file = io.open(path, "w")
    if file == nil then
        return nil, "Failed to write config file: " .. path
    end

    if conf.brain != nil then io.write(file, "brain: " .. conf.brain .. "\n") end
    if conf.editor != nil then io.write(file, "editor: " .. conf.editor .. "\n") end
    if conf.vault != nil then io.write(file, "vault: " .. conf.vault .. "\n") end
    if conf.git != nil then io.write(file, "git: " .. tostring(conf.git) .. "\n") end

    if conf.brains != nil then
        io.write(file, "brains:\n")
        brain_names = {}
        for k, _ in pairs(conf.brains) do
            table.insert(brain_names, k)
        end
        table.sort(brain_names)
        for _, k in ipairs(brain_names) do
            io.write(file, "  " .. k .. ": " .. conf.brains[k] .. "\n")
        end
    end

    io.close(file)
    return true
end

function config.get_brain_path(name)
    cfg = load_config()
    if cfg == nil then return nil end

    if name != nil then
        if cfg["brains"] != nil and cfg["brains"][name] != nil then
            return cfg["brains"][name]
        end
        -- Fallback: if name is not found in "brains" list, checking if it is the "default" one
        -- but if user asked for a specific name and it doesn't exist, we probably should return nil or error?
        -- The robust way is to return nil here so the caller knows it wasn't found.
        return nil
    end

    -- No name provided, try "default" in brains or legacy "brain"
    if cfg["brains"] != nil and cfg["brains"]["default"] != nil then
        return cfg["brains"]["default"]
    end
    
    return cfg["brain"]
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
    if val != nil and (val == true or val == "true") then
        return true
    else
        return false
    end
end

function config.reload()
    cached_config = nil
    return load_config()
end

function config.get_config()
    return load_config()
end

function config.get_named_brains()
    cfg = load_config()
    if cfg == nil then
        return nil
    end

    named = {}
    if cfg["brains"] != nil then
        for name, path in pairs(cfg["brains"]) do
            if name != "default" then
                named[name] = path
            end
        end
    end

    -- Backward compatibility: infer a label for legacy configs that only have brain+vault.
    if next(named) == nil and cfg["vault"] != nil and cfg["brain"] != nil then
        inferred = get_path_label(cfg["vault"])
        if inferred != nil and inferred != "" then
            named[inferred] = cfg["brain"]
        end
    end

    return named
end

function config.set_default_brain(name)
    cfg = load_config()
    if cfg == nil then
        return nil, "Config was not loaded"
    end

    if name == nil or name == "" then
        return nil, "Must provide brain name"
    end

    if cfg["brains"] == nil or cfg["brains"][name] == nil then
        -- Backward compatibility: allow selecting inferred legacy label once, then persist it.
        inferred = nil
        if cfg["vault"] != nil and cfg["brain"] != nil then
            inferred = get_path_label(cfg["vault"])
        end
        if inferred != nil and inferred == name then
            if cfg["brains"] == nil then
                cfg["brains"] = {}
            end
            cfg["brains"][name] = cfg["brain"]
        end
    end

    if cfg["brains"] == nil or cfg["brains"][name] == nil then
        return nil, "Brain '" .. name .. "' not configured."
    end

    selected_path = cfg["brains"][name]
    cfg["brain"] = selected_path
    cfg["brains"]["default"] = selected_path

    status, err = save_config_file(get_config_path(), cfg)
    if status == nil then
        return nil, err
    end

    cached_config = cfg
    return true
end

-- Export the module
return config
