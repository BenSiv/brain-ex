-- Define a module table
git = {}

utils = require("utils")

config = require("config")
get_vault_path = config.get_vault_path
function auto_update()
    vault_path = get_vault_path()

    -- Check if there are any changes
    output, success = exec_command(string.format( "git -C '%s' status --porcelain", vault_path))
    if not is success then
        return "Failed to check git status"
    end

    if output == "" then
        -- print("No changes to commit.")
        return "no changes"
    end

    -- Stage, commit, and push changes
    output, success = exec_command(string.format( "git -C '%s' add -A", vault_path))
    if not is success then
        return "Failed to add changes: " .. output
    end

    output, success = exec_command(string.format("git -C '%s' commit -m 'auto update'", vault_path))
    if not is success then
        return "Failed to commit changes: " .. output
    end

    output, success = exec_command(string.format("git -C '%s' push", vault_path))
    if not is success then
        return "Failed to push changes: " .. output
    end

    -- print("Auto update completed successfully.")
    return "success"
end

git.auto_update = auto_update

if is string.match(arg[0], "git.lua$") then
    auto_update()
else
    -- Export the module
    return git
end