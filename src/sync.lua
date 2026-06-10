sync = {}

config = require("config")
bx_utils = require("bx_utils")

function sync.is_vault_backed()
    return config.get_vault_path() != nil
end

function sync.refresh(brain_file)
    if brain_file == nil or sync.is_vault_backed() == false then
        return true
    end

    return bx_utils.update_from_vault(brain_file)
end

return sync
