-- src/agent_providers/ollama.lua
provider = {}

provider.name = "ollama"

-- Escape shell characters in string
function string.shell_escape(s)
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
    
    command = "cat " .. string.shell_escape(tmpfile) .. " | ollama run " .. string.shell_escape(model)
    result, success = utils.exec_command(command)
    os.remove(tmpfile)
    
    if success == false then
        if result != nil then print(result) end
        return nil, "Failed to execute ollama."
    end
    
    return result, nil
end

return provider
