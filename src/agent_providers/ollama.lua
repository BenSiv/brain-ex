-- src/agent_providers/ollama.lua
provider = {}

provider.name = "ollama"

-- Escape shell characters in string
function shell_escape(s)
    return "'" .. string.gsub(s, "'", "'\\''") .. "'"
end

function provider.generate(model, system_prompt, prompt)
    -- Build the prompt structure
    full_prompt = ""
    if system_prompt and system_prompt != "" then
        full_prompt = full_prompt .. system_prompt .. "\n\n"
    end
    full_prompt = full_prompt .. "Task: " .. prompt

    utils = require("utils")
    tmpfile = os.tmpname()
    utils.write(tmpfile, full_prompt)
    
    command = "cat " .. shell_escape(tmpfile) .. " | ollama run " .. shell_escape(model) .. " 2>&1"
    result, success = utils.exec_command(command)
    os.remove(tmpfile)
    
    if success == false then
        -- Even if it failed, we might have some partial output in result
        error_msg = "Failed to execute ollama."
        if result != nil and result != "" then
            error_msg = error_msg .. " Output: " .. result
        end
        return result, error_msg -- Return result anyway in case it's useful
    end
    
    return result, nil
end

return provider
