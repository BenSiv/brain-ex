-- Define a module table
brain = {}

argparse = require("argparse")
config = require("config")
help = require("help")

function list_brains()
    cfg = config.get_config()
    if cfg == nil then
        return nil, "Failed to load config"
    end

    default_path = config.get_brain_path()
    named = config.get_named_brains()
    if named == nil then
        named = {}
    end
    default_name = "(unlabeled)"

    if default_path == nil then
        print("Default brain: not set")
    else
        for name, path in pairs(named) do
            if path == default_path then
                default_name = name
                break
            end
        end
        print("Default brain: " .. default_name .. " -> " .. default_path)
    end

    print("Configured brains:")
    keys = {}
    for name, _ in pairs(named) do
        table.insert(keys, name)
    end
    table.sort(keys)

    if #keys == 0 then
        print("(none)")
    else
        for _, name in ipairs(keys) do
            path = named[name]
            marker = "-"
            if default_path != nil and path == default_path then
                marker = "*"
            end
            print(marker .. " " .. name .. " -> " .. path)
        end
    end

    return true
end

function use_brain(args)
    brain_name = ""
    if args["name"] != nil then
        brain_name = args["name"]
    end
    if brain_name == "" then
        return nil, "Must provide brain name"
    end

    status, err = config.set_default_brain(brain_name)
    if status == nil then
        return nil, err
    end

    print("Default brain set to '" .. brain_name .. "'")
    return true
end

function do_brain(cmd_args)
    subcommand = cmd_args[1]
    if subcommand != nil and string.sub(subcommand, 1, 1) != "-" then
        valid_subs = {
            ["list"] = true,
            ["use"] = true
        }
        if valid_subs[subcommand] == nil then
            print("Unknown subcommand: " .. subcommand)
            print("Available subcommands: list, use")
            return "success"
        end
        table.insert(cmd_args, 1, "-d")
    else
        subcommand = nil
    end

    arg_string = """
        -d --do arg string false
        -n --name arg string false
    """

    help_string = help.get_help_string(arg[0])
    expected_args = argparse.def_args(arg_string)
    args = argparse.parse_args(cmd_args, expected_args, help_string)
    if args == nil then
        return "success"
    end

    if args["do"] == "list" or args["do"] == nil then
        status, err = list_brains()
        if status == nil then
            msg = "Brain command failed"
            if err != nil then
                msg = err
            end
            print(msg)
            return "error"
        end
        return "success"
    end

    if args["do"] == "use" then
        status, err = use_brain(args)
        if status == nil then
            msg = "Brain command failed"
            if err != nil then
                msg = err
            end
            print(msg)
            return "error"
        end
        return "success"
    end

    print("Unknown subcommand: " .. args["do"])
    print("Available subcommands: list, use")
    return "success"
end

brain.do_brain = do_brain

if string.match(arg[0], "brain.lua$") != nil then
    do_brain(arg)
else
    return brain
end
