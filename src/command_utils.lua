-- Define a module table
local command_utils = {}

require("utils").using("utils")

function command_utils.parse_command_args(arg_definition, help_string)
    local expected_args = def_args(arg_definition)
    local args = parse_args(arg, expected_args, help_string)
    return args
end

function command_utils.handle_command_error(error_msg, help_string)
    print("Error: " .. error_msg)
    if help_string then
        print(help_string)
    end
end

-- Export the module
return command_utils
