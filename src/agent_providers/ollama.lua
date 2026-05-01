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

function provider.embeddings(model, input_text, command_tmpl)
    dkjson = require("dkjson")
    utils = require("utils")

    if command_tmpl != nil and command_tmpl != "" then
        -- Expand template: %m -> model, %t -> input_text (quoted)
        command = string.gsub(command_tmpl, "%%m", model)
        -- For text, we might want to pipe it or escape it.
        -- If %t is in the command, we escape it.
        if string.match(command, "%%t") != nil then
            command = string.gsub(command, "%%t", shell_escape(input_text))
        end

        result, success = utils.exec_command(command)
    else
        -- Default Ollama API
        command = string.format("curl -s http://localhost:11434/api/embeddings -d '{\"model\": \"%s\", \"prompt\": %s}'",
            model, dkjson.encode(input_text))
        result, success = utils.exec_command(command)
    end

    if success == false or result == nil or result == "" then
        return nil, "Failed to get embeddings."
    end

    obj, pos, err = dkjson.decode(result)
    if err != nil then
        -- If not JSON, maybe it's raw numbers? Some commands return raw csv
        -- Try to parse as numbers
        embeddings = {}
        for num in string.gmatch(result, "([-?%d%.eE]+)") do
            table.insert(embeddings, tonumber(num))
        end
        if #embeddings > 0 then
            return embeddings, nil
        end
        return nil, "Failed to decode embeddings: " .. err
    end

    -- API usually returns { "embedding": [...] }
    return obj.embedding or obj.embeddings or obj, nil
end

return provider
