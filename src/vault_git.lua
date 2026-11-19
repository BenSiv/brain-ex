-- Vault git auto-commit/push synchronization.
vault_git = {}

const ext = require("external_tool")
const config = require("config")
const get_vault_path = config.get_vault_path

function auto_update()
    vault_path = get_vault_path()

    output, success, _ = ext.exec(string.format("git -C %s status --porcelain", ext.shell_quote(vault_path)))
    if not success then
        return "Failed to check git status"
    end

    if output == "" then
        return "no changes"
    end

    output, success, _ = ext.exec(string.format("git -C %s add -A", ext.shell_quote(vault_path)))
    if not success then
        return "Failed to add changes: " .. tostring(output)
    end

    output, success, _ = ext.exec(string.format("git -C %s commit -m 'auto update'", ext.shell_quote(vault_path)))
    if not success then
        return "Failed to commit changes: " .. tostring(output)
    end

    output, success, _ = ext.exec(string.format("git -C %s push", ext.shell_quote(vault_path)))
    if not success then
        return "Failed to push changes: " .. tostring(output)
    end

    return "success"
end

vault_git.auto_update = auto_update

if arg != nil and arg[0] != nil and string.match(arg[0], "vault_git.lua$") != nil then
    auto_update()
else
    return vault_git
end
